;;; kele.el --- Spritzy Kubernetes cluster management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Jonathan Jin

;; Author: Jonathan Jin <me@jonathanj.in>

;; Version: 0.2.1
;; Homepage: https://github.com/jinnovation/kele.el
;; Keywords: kubernetes tools
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "28.1") (async "1.9.7") (dash "2.19.1") (f "0.20.0") (ht "2.3") (plz "0.3") (s "1.13.0") (yaml "0.5.1"))

;;; Commentary:

;; kele.el streamlines integration with Kubernetes.  It provides a "base layer"
;; that can be leveraged to build higher-level integrations, e.g. modeline
;; modules and interactive clients.
;;
;; For more details, see: https://jonathanj.in/kele.el.

;;; Code:

(require 'async)
(require 'dash)
(require 'eieio)
(require 'f)
(require 'filenotify)
(require 'ht)
(require 'json)
(require 'plz)
(require 's)
(require 'subr-x)
(require 'transient)
(require 'url-parse)
(require 'yaml)

(require 'kele-fnr)

(declare-function yaml-mode "yaml-mode")

(defgroup kele nil
  "Integration constructs for Kubernetes."
  :group 'external
  :prefix "kele-"
  :link '(url-link "https://github.com/jinnovation/kele.el"))

(defcustom kele-kubeconfig-path
  (expand-file-name (or (getenv "KUBECONFIG") "~/.kube/config"))
  "Path to the kubeconfig file."
  :type 'directory
  :group 'kele)

(defcustom kele-cache-dir
  (f-expand "~/.kube/cache/")
  "Path to the kubectl cache."
  :group 'kele
  :type 'directory)

(defcustom kele-kubectl-executable "kubectl"
  "The kubectl executable to use."
  :type 'string
  :group 'kele)

(defcustom kele-proxy-ttl 60
  "The default time-to-live for ephemeral kubectl proxy processes."
  :type 'integer
  :group 'kele)

(defcustom kele-get-show-instructions t
  "Whether to show usage instructions inside the `kele-get' buffer."
  :group 'kele
  :type 'boolean)

(defcustom kele-resource-default-refresh-interval 60
  "The time-to-live for cached resources.

If a resource is not listed in `kele-resource-refresh-overrides',
its cached values are wiped afther this many seconds."
  :type 'integer
  :group 'kele)

(defcustom kele-resource-refresh-overrides '((namespace . 600))
  "Resource-specific cache time-to-live overrides.

If a resource is listed here, the corresponding value will be
used for cache time-to-live for that resource.  Otherwise,
`kele-resource-default-refresh-interval' is used.

Keys are the singular form of the resource name, e.g. \"pod\" for
pods."
  :type '(alist :key-type symbol :value-type 'integer)
  :group 'kele)

;; TODO (#80): Display in the `kele-get-mode' header what fields were filtered out
(defcustom kele-filtered-fields '((metadata managedFields)
                                  (metadata annotations kubectl.kubernetes.io/last-applied-configuration))
  "Top-level resource fields to never display, e.g. in `kele-get'."
  :type '(repeat (repeat symbol)))

(define-error 'kele-cache-lookup-error
  "Kele failed to find the requested resource in the cache.")
(define-error 'kele-request-error "Kele failed in querying the Kubernetes API")
(define-error 'kele-ambiguous-groupversion-error
  "Found multiple group-versions associated with the given resource")

(cl-defun kele--retry (fn &key (count 5) (wait 1) (timeout 100))
  "Retry FN COUNT times, waiting WAIT seconds between each.

Returns unconditionally after TIMEOUT seconds.

Returns the retval of FN."
  (let ((retval))
    (with-timeout (timeout)
      (let ((count-remaining count))
        (while (and (> count-remaining 0) (not (setq retval (funcall fn))))
          (setq count-remaining (- count-remaining 1))
          (sleep-for wait))))
    retval))

(defun kele--kill-process-quietly (proc &optional _signal)
  "Kill process PROC silently and the associated buffer, suppressing all errors."
  (when proc
    (set-process-sentinel proc nil)
    (set-process-query-on-exit-flag proc nil)
    (let ((kill-buffer-query-functions nil)
          (buf (process-buffer proc)))
      (ignore-errors (kill-process proc))
      (ignore-errors (delete-process proc))
      (ignore-errors (kill-buffer buf)))))

(defconst kele--random-port-range '(1000 9000))

(defun kele--random-port ()
  "Return a random integer within `kele--random-port-range'."
  (+ (car kele--random-port-range) (random (cadr kele--random-port-range))))

(cl-defun kele--proxy-process (context &key port (wait t) (read-only t))
  "Create a new kubectl proxy process for CONTEXT.

The proxy will be opened at PORT (localhost:PORT).  If PORT is
  nil, a random port will be chosen.  It is the caller's
  responsibility to ensure that the port is not occupied.

If READ-ONLY is set, the proxy will only accept read-only
requests.

If WAIT is non-nil, `kele--proxy-process' will wait for the proxy
  to be ready before returning.  This wait is a best effort; the
  proxy's /livez and /readyz endpoints are not guaranteed to
  return 200s by end of wait."
  (let* ((chosen-port (or port (kele--random-port)))
         (s-port (number-to-string chosen-port))
         (proc-name (format "kele: proxy (%s, %s)" context s-port))
         (cmd (list kele-kubectl-executable
                    "--context"
                    context
                    "proxy"
                    "--port"
                    s-port
                    "--reject-methods"
                    (if read-only "\'POST,PUT,PATCH\'" "^$")))
         (proc (make-process
                :name proc-name
                :command cmd
                :buffer (generate-new-buffer (format " *%s*" proc-name))
                :noquery t
                :sentinel
                (lambda (proc _status)
                  (when (zerop (process-exit-status proc))
                    (message "Successfully terminated process: %s" proc-name)
                    (kele--kill-process-quietly proc)))))
         (ready-addr (format "http://localhost:%s/readyz" s-port))
         (live-addr (format "http://localhost:%s/livez" s-port)))
    (when wait
      ;; Give the proxy process some time to spin up, so that curl doesn't
      ;; return error code 7 which to request.el is a "peculiar error"
      (sleep-for 2)
      (kele--retry (lambda ()
                     ;; /readyz and /livez can sometimes return nil, maybe when
                     ;; the proxy is just starting up. Add retries for these.
                     (when-let* ((resp-ready (plz 'get ready-addr :as 'response))
                                 (resp-live (plz 'get live-addr :as 'response))
                                 (status-ready (plz-response-status resp-ready))
                                 (status-live (plz-response-status resp-live)))
                       (and (= 200 status-ready) (= 200 status-live))))
                   :wait 2
                   :count 10))
    proc))

(cl-defun kele-kubectl-do (&rest args)
  "Execute kubectl with ARGS."
  (let ((cmd (append (list kele-kubectl-executable)
                     `("--kubeconfig" ,kele-kubeconfig-path)
                     args)))
    (make-process
     :name (format "kele: %s" (s-join " " cmd))
     :command cmd
     :noquery t)))

(cl-defgeneric kele--cache-update (&optional _)
  "Update in response to activity in the underlying filesystem.

Ideally this should be an asynchronous process.  This function
should be suitable for use as part of a file-watcher.")

(cl-defgeneric kele--cache-start (&key bootstrap)
  "Start watching the file system.

If BOOTSTRAP is non-nil, the implementation should also perform a
bootstrapping update, e.g. `kele--cache-update'.")

(cl-defgeneric kele--cache-stop ()
  "Stop watching the file system.")

(defclass kele--discovery-cache ()
  ((contents
    :documentation
    "Alist mapping contexts to the discovered APIs.

Key is the host name and the value is a list of all the
   APIGroupLists and APIResourceLists found in said cache.")
   (filewatch-id
    :documentation "The ID of the file watcher."))
  "Track the Kubernetes discovery cache.

A class for loading a Kubernetes discovery cache and keeping it
in sync with the filesystem.")

(defclass kele--kubeconfig-cache ()
  ((contents
    :documentation "The loaded kubeconfig contents.")
   (filewatch-id
    :documentation "The ID of the file watcher."))
  "Track the kubeconfig cache.

A class for loading kubeconfig contents and keeping them in sync
with the filesystem.")

(defun kele--get-host-for-context (&optional context)
  "Get host for CONTEXT."
  (let* ((server (let-alist (kele--context-cluster (or context (kele-current-context-name)))
                   .cluster.server))
         (host (url-host (url-generic-parse-url server)))
         (port (url-portspec (url-generic-parse-url server))))
    (s-concat host (if port (format ":%s" port) ""))))

(cl-defmethod kele--get-resource-lists-for-context ((cache kele--discovery-cache)
                                                    &optional context)
  "Get all resource lists for CONTEXT from CACHE."
  (alist-get
   (s-replace ":" "_" (kele--get-host-for-context (or context (kele-current-context-name))))
   (oref cache contents)
   nil nil #'equal))

(cl-defmethod kele--get-groupversions-for-type ((cache kele--discovery-cache)
                                                type
                                                &key context)
  "Look up the groupversions for a given resource TYPE in CACHE.

TYPE is expected to be the plural name of the resource.

If CONTEXT is nil, use the current context."
    (->> (kele--get-resource-lists-for-context cache (or context (kele-current-context-name)))
         (-filter (lambda (api-resource-list)
                   (->> (alist-get 'resources api-resource-list)
                        (-any (lambda (resource)
                                (equal (alist-get 'name resource) type))))))
         (-map (-partial #'alist-get 'groupVersion))
         (-sort (lambda (a _) (equal a "v1")))))

(cl-defmethod kele--resource-namespaced-p ((cache kele--discovery-cache)
                                           group-version
                                           type
                                           &key context)
  "Look up the namespaced-ness of GROUP-VERSION TYPE in CACHE.

If CONTEXT is not provided, the current context is used."
  (if-let ((namespaced-p
            (->> (kele--get-resource-lists-for-context cache (or context (kele-current-context-name)))
                 (-first (lambda (resource-list) (equal (alist-get 'groupVersion resource-list) group-version)))
                 (alist-get 'resources)
                 (-first (lambda (resource) (equal (alist-get 'name resource) type)))
                 (alist-get 'namespaced))))
      (not (eq :false namespaced-p))
    (signal 'kele-cache-lookup-error `(,context ,group-version ,type))))

(cl-defmethod kele--cache-update ((cache kele--discovery-cache) &optional _)
  "Update CACHE with the values from `kele-cache-dir'.

This is done asynchronously.  To wait on the results, pass the
retval into `async-wait'."
  (let* ((progress-reporter (make-progress-reporter "Pulling discovery cache..."))
         (func-complete (lambda (res)
                          (oset cache contents res)
                          (progress-reporter-done progress-reporter))))
    (async-start `(lambda ()
                    (add-to-list 'load-path (file-name-directory ,(locate-library "dash")))
                    (add-to-list 'load-path (file-name-directory ,(locate-library "f")))
                    (add-to-list 'load-path (file-name-directory ,(locate-library "s")))
                    (add-to-list 'load-path (file-name-directory ,(locate-library "yaml")))
                    (require 'f)
                    (require 'json)
                    (require 'yaml)
                    ,(async-inject-variables "kele-cache-dir")
                    (->> (f-entries (f-join kele-cache-dir "discovery"))
                         (-map (lambda (dir)
                                 (let* ((api-list-files (f-files dir
                                                                 (lambda (file)
                                                                   (equal (f-ext file) "json"))
                                                                 t))
                                        (api-lists (-map (lambda (file)
                                                           (json-parse-string (f-read file)
                                                                              :object-type 'alist
                                                                              :array-type 'list))
                                                         api-list-files))
                                        (key (f-relative dir (f-join kele-cache-dir "discovery"))))
                                   `(,key . ,api-lists))))))
                 func-complete)))

(cl-defmethod kele--resource-has-verb-p ((cache kele--discovery-cache)
                                         group-version kind verb &key context)
  (-contains-p (->> (kele--get-resource-lists-for-context
                     cache
                     (or context (kele-current-context-name)))
                    (-first (lambda (resource-list)
                              (equal (alist-get 'groupVersion resource-list)
                                     group-version)))
                    (alist-get 'resources)
                    (-first (lambda (resource)
                              (equal (alist-get 'name resource)
                                     kind)))
                    (alist-get 'verbs))
               (if (stringp verb) verb (symbol-name verb))))

(cl-defmethod kele--cache-start ((cache kele--discovery-cache) &key bootstrap)
  "Start file-watch for CACHE.

If BOOTSTRAP is non-nil, perform an initial read."
  (oset cache
        filewatch-id
        (kele--fnr-add-watch
         (f-join kele-cache-dir "discovery/")
         '(change)
         (-partial #'kele--cache-update cache)))
  (when bootstrap
    (kele--cache-update cache)))

(cl-defmethod kele--cache-stop ((cache kele--discovery-cache))
  "Stop file-watch for CACHE."
  (kele--fnr-rm-watch (oref cache filewatch-id)))

(defvar kele--global-kubeconfig-cache (kele--kubeconfig-cache))
(defvar kele--global-discovery-cache (kele--discovery-cache))

(cl-defmethod kele--cache-stop ((cache kele--kubeconfig-cache))
  "Stop watching `kele-kubeconfig-path' for contents to write to CACHE."
  (file-notify-rm-watch (oref cache filewatch-id)))

(cl-defmethod kele--cache-update ((cache kele--kubeconfig-cache) &optional _)
  "Update CACHE with the values from `kele-kubeconfig-path'.

This is done asynchronously.  To wait on the results, pass the
retval into `async-wait'."
  (let* ((progress-reporter (make-progress-reporter "Pulling kubeconfig contents..."))
         (func-complete (lambda (config)
                          (oset cache contents config)
                          (progress-reporter-done progress-reporter))))
    (async-start `(lambda ()
                    ;; TODO: How to just do all of these in one fell swoop?
                    (add-to-list 'load-path (file-name-directory ,(locate-library "yaml")))
                    (add-to-list 'load-path (file-name-directory ,(locate-library "f")))
                    (add-to-list 'load-path (file-name-directory ,(locate-library "s")))
                    (add-to-list 'load-path (file-name-directory ,(locate-library "dash")))
                    (require 'yaml)
                    (require 'f)
                    ,(async-inject-variables "kele-kubeconfig-path")
                    (yaml-parse-string (f-read kele-kubeconfig-path)
                                       :object-type 'alist
                                       :sequence-type 'list))
                 func-complete)))

(cl-defmethod kele--cache-start ((cache kele--kubeconfig-cache) &key bootstrap)
  "Start watching `kele-kubeconfig-path' for CACHE.

If BOOTSTRAP is non-nil, will perform an initial load of the
contents."
  (oset cache
        filewatch-id
        (file-notify-add-watch
         kele-kubeconfig-path
         '(change)
         (-partial #'kele--cache-update cache)))
  (when bootstrap
    (kele--cache-update cache)))

(defvar kele--context-proxy-ledger nil
  "An alist mapping contexts to their corresponding proxy processes.

Keys are context names.  Values are alists with the keys `proc',
`timer', and `port'.  If nil, there is no active proxy for that
context.

The values at each are as follows:

  - The value at `proc' is the kubectl proxy process;

  - `timer' is a timer object that terminates the proxy and
    cleans up the proxy process.  If nil, the proxy process will
    not be automatically cleaned up and it is user responsibility
    to do so;

  - `port' is the port that the proxy was opened on.")

(cl-defun kele--get-resource-types-for-context (context-name &key verb)
  "Retrieve the names of all resource types for CONTEXT-NAME.

If VERB provided, returned resource types will be restricted to
those that support the given verb."
  (->> (kele--get-resource-lists-for-context kele--global-discovery-cache
                                             (or context-name (kele-current-context-name)))
       (-filter (lambda (resource-list) (equal (alist-get 'kind resource-list) "APIResourceList")))
       (-map (lambda (list) (alist-get 'resources list)))
       (-flatten-n 1)
       (-filter (lambda (resource)
                  (if verb
                      (-contains-p (alist-get 'verbs resource)
                                   (if (stringp verb) verb (symbol-name verb)))
                    t)))
       (-map (lambda (resource) (alist-get 'name resource)))
       (-uniq)))

(defun kele-current-context-name ()
  "Get the current context name.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (alist-get 'current-context (oref kele--global-kubeconfig-cache contents)))

(defun kele--default-namespace-for-context (context)
  "Get the defualt namespace for CONTEXT."
  (-if-let* (((&alist 'context (&alist 'namespace namespace))
              (-first (lambda (elem)
                        (string= (alist-get 'name elem) context))
                      (alist-get 'contexts (oref kele--global-kubeconfig-cache contents)))))
      namespace))

(defun kele-current-namespace ()
  "Get the current context's default namespace.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (kele--default-namespace-for-context (kele-current-context-name)))

(defun kele-status-simple ()
  "Return a simple status string suitable for modeline display."
  (let ((status (if (not (or (kele-current-context-name) (kele-current-namespace)))
                    "--"
                  (concat (kele-current-context-name)
                          (if (kele-current-namespace)
                              (concat "(" (kele-current-namespace) ")")
                            "")))))
    (concat "k8s:" status)))

(defconst kele--awesome-tray-module '("kele" . (kele-status-simple nil)))

(defun kele-context-names ()
  "Get the names of all known contexts."
  (-map
   (lambda (elem) (alist-get 'name elem))
   (alist-get 'contexts (oref kele--global-kubeconfig-cache contents))))

(defun kele--context-cluster (context-name)
  "Get the cluster metadata for the context named CONTEXT-NAME."
  (-first (lambda (elem) (string= (alist-get 'name elem)
                                  (kele--context-cluster-name context-name)))
          (alist-get 'clusters (oref kele--global-kubeconfig-cache contents))))

(defun kele--context-cluster-name (context-name)
  "Get the name of the cluster of the context named CONTEXT-NAME."
  (if-let ((context (-first (lambda (elem) (string= (alist-get 'name elem) context-name))
                            (alist-get 'contexts (oref kele--global-kubeconfig-cache contents)))))
      (alist-get 'cluster (alist-get 'context context))
    (error "Could not find context of name %s" context-name)))

(defun kele--context-annotate (context-name)
  "Return annotation text for the context named CONTEXT-NAME."
  (let* ((context (-first (lambda (elem)
                            (string= (alist-get 'name elem) context-name))
                          (alist-get 'contexts (oref kele--global-kubeconfig-cache contents))))
         (cluster-name (or (alist-get 'cluster (alist-get 'context context)) ""))
         (cluster (-first (lambda (elem)
                            (string= (alist-get 'name elem) cluster-name))
                          (-concat (alist-get 'clusters (oref kele--global-kubeconfig-cache contents)) '())))
         (server (or (alist-get 'server (alist-get 'cluster cluster)) ""))
         (proxy-active-p (assoc (intern context-name) kele--context-proxy-ledger))
         (proxy-status (if proxy-active-p
                           (propertize "Proxy ON" 'face 'warning)
                         (propertize "Proxy OFF" 'face 'shadow))))
    (format " %s%s, %s, %s%s"
            (propertize "(" 'face 'completions-annotations)
            (propertize cluster-name 'face 'completions-annotations)
            (propertize server 'face 'completions-annotations)
            proxy-status
            (propertize ")" 'face 'completions-annotations))))

(cl-defun kele--resources-complete (str pred action &key cands category)
  "Complete input for selection of resources.

STR, PRED, and ACTION are as defined in completion functions.

CANDS is the collection of completion candidates.

CATEGORY is the category the candidates should be categorized
as."
  (if (eq action 'metadata)
      `(metadata (category . ,(or category 'kele-resource)))
    (complete-with-action action cands str pred)))

(defun kele-namespace-switch-for-context (context namespace)
  "Switch to NAMESPACE for CONTEXT."
  (interactive (let ((context (completing-read "Context: " #'kele--contexts-complete)))
                 (list context
                       (completing-read (format "Namespace (%s): " context)
                                        (-cut kele--resources-complete <> <> <>
                                              :cands (kele--get-namespaces
                                                      context)
                                              :category 'kele-namespace)))))
  (kele-kubectl-do "config" "set-context" context "--namespace" namespace))

(defun kele-namespace-switch-for-current-context (namespace)
  "Switch to NAMESPACE for the current context."
  (interactive
   (list
    (completing-read
     (format "Namespace (%s): " (kele-current-context-name))
     (-cut kele--resources-complete <> <> <>
           :cands (kele--get-namespaces (kele-current-context-name))
           :category 'kele-namespace))))
  (kele-namespace-switch-for-context (kele-current-context-name) namespace))

;; TODO
(defun kele--namespace-annotate (_namespace-name)
  "Return annotation text for the namespace named NAMESPACE-NAME."
  "")

(defun kele--contexts-complete (str pred action)
  "Complete input for selection of contexts.

STR, PRED, and ACTION are as defined in completion functions."
  (if (eq action 'metadata)
      '(metadata (annotation-function . kele--context-annotate)
                 (category . kele-context))
    (complete-with-action action (kele-context-names) str pred)))

(defmacro kele--with-progress (msg &rest body)
  "Execute BODY with a progress reporter using MSG.

Returns the last evaluated value of BODY."
  (declare (indent defun))
  `(let ((prog (make-progress-reporter ,msg))
         (res (progn ,@body)))
    (progress-reporter-done prog)
    res))

(defun kele-context-switch (context)
  "Switch to CONTEXT."
  (interactive (list (completing-read "Context: " #'kele--contexts-complete)))
  (kele--with-progress (format "Switching to use context `%s'..." context)
    (kele-kubectl-do "config" "use-context" context)))

(defun kele-context-rename (old-name new-name)
  "Rename context named OLD-NAME to NEW-NAME."
  (interactive (list (completing-read "Context to rename: "
                                      #'kele--contexts-complete
                                      nil t nil nil (kele-current-context-name))
                     (read-from-minibuffer "Rename to: ")))
  ;; TODO: This needs to update `kele--context-proxy-ledger' as well.
  (kele-kubectl-do "config" "rename-context" old-name new-name))

(cl-defun kele-proxy-stop (context)
  "Clean up the proxy for CONTEXT."
  (interactive (list (completing-read "Stop proxy for context: " #'kele--contexts-complete)))
  (-let (((&alist 'proc proc 'timer timer) (alist-get (intern context) kele--context-proxy-ledger)))
    (kele--kill-process-quietly proc)
    (when timer (cancel-timer timer)))
  (setq kele--context-proxy-ledger (assoc-delete-all (intern context) kele--context-proxy-ledger)))

(cl-defun kele-proxy-start (context &key port (ephemeral t))
  "Start a proxy process for CONTEXT at PORT.

If EPHEMERAL is non-nil, the proxy process will be cleaned up
after a certain amount of time.

If PORT is nil, a random port will be chosen.

Returns the proxy process."
  (interactive (list (completing-read "Start proxy for context: " #'kele--contexts-complete)
                     :port nil
                     :ephemeral t))
  ;; TODO: Throw error if proxy already active for context
  (kele--with-progress (format "Starting proxy server process for `%s'..." context)
    (let* ((selected-port (or port (kele--random-port)))
           (key (intern context))
           (proc (kele--proxy-process context :port selected-port))
           (cleanup (when ephemeral
                      (run-with-timer kele-proxy-ttl nil #'kele-proxy-stop context)))
           ;; TODO: Define a struct for this
           (entry `((proc . ,proc)
                    (timer . ,cleanup)
                    (port . ,selected-port))))
      (add-to-list 'kele--context-proxy-ledger `(,key . ,entry))
      entry)))

(defun kele--proxy-enabled-p (context)
  "Return non-nil if proxy server process active for CONTEXT."
  (alist-get (intern context) kele--context-proxy-ledger))

(defun kele-proxy-toggle (context)
  "Start or stop proxy server process for CONTEXT."
  (interactive (list (completing-read
                      "Start/stop proxy for context: "
                      #'kele--contexts-complete)))
  (funcall
   (if (kele--proxy-enabled-p context) #'kele-proxy-stop #'kele-proxy-start)
   context))

(cl-defun kele--ensure-proxy (context)
  "Return a proxy process for CONTEXT, creating one if needed."
  (if-let* ((entry (alist-get (intern context) kele--context-proxy-ledger)))
      entry
    (kele-proxy-start context)))

(defvar kele--context-resources nil
  "An alist mapping contexts to their cached resources.

Values are: (RESOURCE-NAME . (list RESOURCE)).")

(defvar kele--context-namespaces nil
  "An alist mapping contexts to their constituent namespaces.

If value is nil, the namespaces need to be fetched directly.")

(defun kele--clear-namespaces-for-context (context)
  "Clear the stored namespaces for CONTEXT."
  (setq kele--context-namespaces
        (assoc-delete-all (intern context) kele--context-namespaces)))

(defun kele--get-namespaces (context)
  "Get namespaces for CONTEXT.

If not cached, will fetch and cache the namespaces."
  (if-let ((namespaces (alist-get (intern context) kele--context-namespaces)))
      namespaces
    (apply #'kele--cache-namespaces context
           (kele--fetch-resource-names nil "v1" "namespaces" :context context))))

(defun kele--get-cache-ttl-for-resource (resource)
  "Get the cache TTL for RESOURCE."
  (or (alist-get resource kele-resource-refresh-overrides)
      kele-resource-default-refresh-interval))

(defun kele--cache-namespaces (context &rest namespace-names)
  "Cache NAMESPACE-NAMES as the associated namespaces for CONTEXT.

The cache has a TTL as defined by
`kele-resource-refresh-overrides' and
`kele-resource-default-refresh-interval'.

Returns the passed-in list of namespaces."
  (add-to-list 'kele--context-namespaces `(,(intern context) . ,namespace-names))
  (run-with-timer
   (kele--get-cache-ttl-for-resource 'namespace)
   nil
   #'kele--clear-namespaces-for-context
   context)
  namespace-names)

(cl-defstruct (kele--resource-container
               (:constructor kele--resource-container-create)
               (:copier nil))
  "Container associating a Kubernetes RESOURCE with its CONTEXT and NAMESPACE.

RESOURCE is expected to be an alist representing the Kubernetes
object.

RETRIEVAL-TIME denotes the time at which RESOURCE was retrieved.
"
  resource
  context
  namespace
  kind
  retrieval-time)

(cl-defun kele--get-resource (kind name &key group version context namespace)
  "Get resource KIND by NAME.

KIND should be the plural form of the kind's name, e.g. \"pods\"
instead of \"pod.\"

If GROUP and VERSION are nil, the function will look up the
possible group-versions for the resource KIND. If there is more
than one group-version associated with the resource KIND, the
function will signal an error.

If GROUP is nil, look up KIND in the core API group.

If CONTEXT is nil, use the current context.

If NAMESPACE is nil and the resource KIND is namespaced, use the
default namespace of the given CONTEXT.

If NAMESPACE is provided for a non-namespaced resource KIND,
throws an error."
  (let* ((context (or context (kele-current-context-name)))
         (time (current-time-string))
         (group-versions
          (cond
           ((and group version) (list (format "%s/%s" group version)))
           ((and version (not group)) (list version))
           (t (kele--get-groupversions-for-type kele--global-discovery-cache
                                                kind
                                                :context context)))))

    (when (> (length group-versions) 1)
      (signal 'kele-ambiguous-groupversion-error group-versions))
    (when (and namespace (not (kele--resource-namespaced-p
                               kele--global-discovery-cache
                               (car group-versions)
                               kind
                               :context context)))
      (user-error "Namespace `%s' specified for un-namespaced resource `%s'; remove namespace and try again" namespace kind))

    (-let* ((gv (car group-versions))
            (namespace (and (kele--resource-namespaced-p
                             kele--global-discovery-cache
                             gv
                             kind
                             :context context)
                            (or namespace (kele--default-namespace-for-context context))))
            (url-gv (if (s-contains-p "/" gv)
                        (format "apis/%s" gv)
                      (format "api/%s" gv)))
            (url-res (format "%s/%s" kind name))
            (url-all (concat url-gv "/"
                             (if namespace (format "namespaces/%s/" namespace) "")
                             url-res))
            ((&alist 'port port) (kele--ensure-proxy context))
            (url (format "http://localhost:%s/%s" port url-all)))
      (condition-case err
          (kele--resource-container-create
           :resource (kele--retry (lambda () (plz 'get url :as #'json-read)))
           :context context
           :kind kind
           :namespace namespace
           :retrieval-time time)
        (error (signal 'kele-request-error (error-message-string err)))))))

(cl-defstruct (kele--resource-buffer-context
               (:constructor kele--resource-buffer-context-create)
               (:copier nil)
               (:include kele--resource-container))
  "Contextual metadata for a `kele-get-mode' buffer."
  filtered-paths)

(defvar kele--current-resource-buffer-context)

(defun kele--quit-and-kill (&optional window)
  "Quit WINDOW and kill its buffer."
  (interactive)
  (quit-window t window))

(defvar kele--get-mode-command-descriptions
  '((quit-window . "quit window")
    (kele--quit-and-kill . "quit window, killing buffer")
    (kele--refetch . "re-fetch the resource")))

(define-minor-mode kele-get-mode
  "Enable some Kele features in resource-viewing buffers.

Kele resource buffers are created when you run `kele-get'.  They
show the requested Kubernetes object manifest.

\\{kele-get-mode-map}"
  :group 'kele
  :interactive nil
  :lighter "Kele Get"
  :keymap `((,(kbd "q") . quit-window)
            (,(kbd "Q") . kele--quit-and-kill)
            (,(kbd "U") . kele--refetch))
  (read-only-mode 1))

(defun kele--refetch ()
  "Refetches the currently displayed resource."
  (interactive)
  (cl-assert kele-get-mode
             nil
             "`kele--refetch' is only meaningful in a `kele-get-mode' buffer; refusing to invoke")
  (-let* ((ctx kele--current-resource-buffer-context)
          ((&alist 'kind kind
                   'apiVersion apiVersion
                   'metadata (&alist 'name name
                                     'namespace namespace))
           (kele--resource-buffer-context-resource ctx))
          (context (kele--resource-buffer-context-context ctx)))
    (kele--with-progress (format "Re-fetching resource `%s/%s' (namespace: %s, context: %s)..."
                                 kind
                                 name
                                 namespace
                                 context)
      (kele--render-object
       (kele--get-resource (kele--resource-buffer-context-kind ctx)
                           name
                           :group (if (s-contains-p "/" apiVersion)
                                      (car (s-split "/" apiVersion))
                                    nil)
                           :version (if (s-contains-p "/" apiVersion)
                                        (cadr (s-split "/" apiVersion))
                                      apiVersion)
                           :namespace namespace
                           :context context)
       (current-buffer)))))

(defun kele--get-insert-header ()
  "Insert header into a `kele-get-mode' buffer."
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-min))
      (when (boundp 'kele--current-resource-buffer-context)
        (insert (propertize (format "# Context: %s\n"
                                    (kele--resource-buffer-context-context
                                     kele--current-resource-buffer-context))
                            'font-lock-face 'font-lock-comment-face))
        (insert (propertize (format "# Retrieval time: %s\n"
                                    (kele--resource-buffer-context-retrieval-time
                                     kele--current-resource-buffer-context))
                            'font-lock-face 'font-lock-comment-face)))
      (when kele-get-show-instructions
        (insert "#\n")
        (insert "# Keybindings:\n")
        (pcase-dolist (`(,cmd . ,desc) kele--get-mode-command-descriptions)
          (insert (format (propertize "# %s %s\n"
                                      'font-lock-face 'font-lock-comment-face)
                          (s-pad-right
                           10
                           " "
                           (substitute-command-keys (format "\\[%s]" cmd)))
                          desc)))))))

(add-hook 'kele-get-mode-hook #'kele--get-insert-header t)

(defun kele--groupversion-string (group version)
  (if group (concat group "/" version) version))

;; TODO (#72): Allow for injecting the proxy dependency.
;; This would allow for consumers to create their own proxy, e.g. to start it
;; async while accepting user input, and defer its use to here.
;;
;; :proxy value should be assumed to be either a proxy container struct or a
;; future that's expected to return one.
(cl-defun kele--fetch-resource-names (group version kind &key namespace context)
  "Fetch names of resources belonging to GROUP, VERSION, and KIND.

If NAMESPACE is provided, return only resources belonging to that namespace.  If
NAMESPACE is provided for non-namespaced KIND, throws an error.

If CONTEXT is not provided, use the current context."
  (when (and namespace
             (not (kele--resource-namespaced-p
                   kele--global-discovery-cache
                   (kele--groupversion-string group version)
                   kind)))
    (signal 'user-error '()))

  (-if-let* (((&alist 'port port) (kele--ensure-proxy
                                   (or context (kele-current-context-name))))
             (url (format "http://localhost:%s/%s/%s"
                          port
                          (if group
                              (format "apis/%s/%s" group version)
                            (format "api/%s" version))
                          kind))
             (data (kele--retry (lambda () (plz 'get url :as #'json-read))))
             ((&alist 'items items) data))
      (->> (append items '())
           (-filter (lambda (item)
                      (if (not namespace) t
                        (let-alist item
                          (equal .metadata.namespace namespace)))))
           (-map (lambda (item)
                   (let-alist item
                     .metadata.name))))
    (signal 'error (format "Failed to fetch %s/%s/%s" group version kind))))

(cl-defun kele-get (kind name &key group version context namespace)
  "Get resource KIND by NAME and display it in a buffer.

KIND should be the plural form of the kind's name, e.g. \"pods\"
instead of \"pod.\"

If GROUP and VERSION are nil, the function will look up the
possible group-versions for the resource KIND. If there is more
than one group-version associated with the resource KIND, the
function will signal an error.

If GROUP is nil, look up KIND in the core API group.

If CONTEXT is nil, use the current context.

If NAMESPACE is nil and the resource KIND is namespaced, use the
default namespace of the given CONTEXT.

If NAMESPACE is provided for a non-namespaced resource KIND,
throws an error."
  ;; TODO (#72): Start proxy server asynchronously here; await on it right when it's
  ;; needed
  (interactive (let* ((ctx (kele-current-context-name))
                      (kind (completing-read
                             "Kind: "
                             (kele--get-resource-types-for-context ctx :verb 'get)))
                      (gvs (kele--get-groupversions-for-type
                            kele--global-discovery-cache
                            kind
                            :context ctx))
                      (gv (if (= (length gvs) 1)
                              (car gvs)
                            (completing-read (format "Desired group-version of `%s': "
                                                     kind)
                                             gvs)))
                      (group (when (s-contains-p "/" gv) (car (s-split "/"
                                                                       gv))))
                      (version (if (s-contains-p "/" gv) (cadr (s-split "/" gv))
                                 gv))
                      (ns (if (not (kele--resource-namespaced-p
                                    kele--global-discovery-cache
                                    gv
                                    kind
                                    :context ctx))
                              nil
                            (completing-read (format "Namespace to get `%s/%s' from: "
                                                     gv
                                                     kind)
                                             (kele--get-namespaces ctx)))))
                 (list kind
                       (completing-read
                        "Name: "
                        (-cut kele--resources-complete <> <> <>
                              :cands (kele--fetch-resource-names group version kind :namespace ns :context ctx)))
                       :group group
                       :version version
                       :context ctx
                       :namespace ns)))
  (kele--render-object (kele--get-resource kind name
                                           :group group
                                           :version version
                                           :namespace namespace
                                           :context context)))

(cl-defun kele--render-object (object &optional buffer)
  "Render OBJECT in a buffer as YAML.

Filters out fields according to `kele-filtered-fields'.

If BUFFER is provided, renders into it.  Otherwise, a new buffer
will be created.

OBJECT is either an alist representing a Kubernetes object, or a
`kele--resource-container'.  If the latter, buffer will have
context and namespace in its name."
  (cl-assert (and object (if (kele--resource-container-p object)
                             (kele--resource-container-resource object)
                           t)))
  (let* ((buf-name (concat " *kele: "
                           (if (kele--resource-container-p object)
                               (concat
                                (kele--resource-container-context object)
                                (and (kele--resource-container-namespace object)
                                     (format "(%s)"
                                             (kele--resource-container-namespace
                                              object)))
                                ": "))
                           (let-alist (if (kele--resource-container-p object)
                                          (kele--resource-container-resource object)
                                        object)
                             (format "%s/%s" .kind .metadata.name))
                           "*"))
         (buf (or buffer (get-buffer-create buf-name)))
         (obj (if (kele--resource-container-p object)
                  (kele--resource-container-resource object)
                object))
         (filtered-obj (-reduce-from (lambda (o keys)
                                       (apply #'kele--prune o keys))
                                     obj
                                     kele-filtered-fields)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert (yaml-encode filtered-obj))
        (whitespace-cleanup)
        (goto-char (point-min)))

      (if (featurep 'yaml-mode) (yaml-mode)
        (message "[kele] For syntax highlighting, install `yaml-mode'."))

      (when (kele--resource-container-p object)
        (setq-local kele--current-resource-buffer-context
                    (kele--resource-buffer-context-create
                     :context (kele--resource-container-context object)
                     :kind (kele--resource-container-kind object)
                     :retrieval-time (kele--resource-container-retrieval-time object)
                     :resource (kele--resource-container-resource object)
                     :namespace (kele--resource-container-namespace object)))
        (put 'kele--current-resource-buffer-context 'permanent-local t))

      (kele-get-mode 1))
    (select-window (display-buffer buf))))

(defun kele--prune (alist &rest keys)
  "Delete the sub-tree of ALIST corresponding to KEYS."
  (let ((prev)
        (curr alist))
    (dolist (key keys)
      (setq prev curr)
      (setq curr (if-let ((list-p (listp curr))
                          (res (assq key curr)))
                     (cdr res)
                   nil)))
    (when curr
      (assq-delete-all (car (last keys)) prev))
    alist))

(defvar kele--context-keymap nil
  "Keymap for actions on Kubernetes contexts.

Only populated if Embark is installed.")

(defvar kele--namespace-keymap nil
  "Keymap for actions on Kubernetes namespaces.

Only populated if Embark is installed.")

(defconst kele--embark-keymap-entries '((kele-context . kele--context-keymap)
                                        (kele-namespace . kele--namespace-keymap)))

(defun kele--setup-embark-maybe ()
  "Optionally set up Embark integration."
  (when (featurep 'embark)
    (with-suppressed-warnings ((free-vars embark-keymap-alist embark-general-map))
      (setq kele--context-keymap (let ((map (make-sparse-keymap)))
                                   (define-key map "s" #'kele-context-switch)
                                   (define-key map "r" #'kele-context-rename)
                                   (define-key map "p" #'kele-proxy-toggle)
                                   (define-key map "n" #'kele-namespace-switch-for-context)
                                   (make-composed-keymap map embark-general-map)))
      (setq kele--namespace-keymap (let ((map (make-sparse-keymap)))
                                     (define-key map "s" #'kele-namespace-switch-for-current-context)
                                     (make-composed-keymap map embark-general-map)))
      (dolist (entry kele--embark-keymap-entries)
        (add-to-list 'embark-keymap-alist entry)))))

(defun kele--teardown-embark-maybe ()
  "Optionally tear down Embark integration."
  (when (featurep 'embark)
    (with-suppressed-warnings ((free-vars embark-keymap-alist))
      (dolist (entry kele--embark-keymap-entries)
        (delete entry embark-keymap-alist)))))

(defun kele--enable ()
  "Enables Kele functionality."
  ;; FIXME: Update the watcher when `kele-kubeconfig-path' changes.
  (kele--cache-start kele--global-kubeconfig-cache :bootstrap t)
  ;; FIXME: Update the watcher when `kele-cache-dir' changes.
  (kele--cache-start kele--global-discovery-cache :bootstrap t)

  (kele--setup-embark-maybe)
  (if (featurep 'awesome-tray)
      (with-suppressed-warnings ((free-vars awesome-tray-module-alist))
        (add-to-list 'awesome-tray-module-alist kele--awesome-tray-module))))

(defun kele--disable ()
  "Disable Kele functionality."
  (kele--cache-stop kele--global-kubeconfig-cache)
  (kele--cache-stop kele--global-discovery-cache)
  (kele--teardown-embark-maybe)
  (if (featurep 'awesome-tray)
      (with-suppressed-warnings ((free-vars awesome-tray-module-alist))
        (delete kele--awesome-tray-module awesome-tray-module-alist))))

;;;###autoload
(define-minor-mode kele-mode
  "Minor mode to enable listening on Kubernetes configs."
  :global t
  :group 'kele
  :lighter nil
  (if (not kele-mode)
      (kele--disable)
    (kele--enable)))

(transient-define-prefix kele-context (context)
  "Work with a Kubernetes CONTEXT."
  [:description
   (lambda () (format "Contexts (current: %s)"
                      (propertize (oref transient--prefix scope) 'face 'warning)))
   ("s" "Switch to..." kele-context-switch)
   ("r" "Rename" kele-context-rename)
   ("n" kele-namespace-switch-for-current-context
    :description (lambda () (format "Change default namespace of %s to..."
                                    (propertize (oref transient--prefix scope)
                                                'face 'warning))))]
  (interactive (list (kele-current-context-name)))
  (transient-setup 'kele-context nil nil :scope context))

(provide 'kele)

;;; kele.el ends here
