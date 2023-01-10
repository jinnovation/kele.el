;;; kele.el --- Interface with Kubernetes -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2023 Jonathan Jin

;; Author: Jonathan Jin <me@jonathanj.in>

;; Version: 0.1.1
;; Homepage: https://github.com/jinnovation/kele.el
;; Keywords: kubernetes tools
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "27.1") (async "1.9.7") (dash "2.19.1") (f "0.20.0") (ht "2.3") (plz "0.3") (request "0.3.2") (yaml "0.5.1"))

;;; Commentary:

;; kele.el streamlines integration with Kubernetes.  It provides a "base layer"
;; that can be leveraged to build higher-level integrations, e.g. modeline
;; modules and interactive clients.
;;
;; For more details, see: https://github.com/jinnovation/kele.el.

;;; Code:

(require 'async)
(require 'dash)
(require 'eieio)
(require 'f)
(require 'filenotify)
(require 'ht)
(require 'json)
(require 'plz)
(require 'subr-x)
(require 'url-parse)
(require 'yaml)

(require 'kele-fnr)

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
  (f-join (f-dirname kele-kubeconfig-path) "cache")
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
                :buffer (generate-new-buffer (format "*%s*" proc-name))
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

;; TODO: At some point it might become necessary to return select metadata about
;; the resources, e.g. group and version
(defun kele--get-resource-types-for-context (context-name)
  "Retrieve the names of all resource types for CONTEXT-NAME."
  (-if-let* (((&alist 'cluster (&alist 'server server)) (kele--context-cluster context-name))
             (host (url-host (url-generic-parse-url server))))
      (->> (alist-get host (oref kele--global-discovery-cache contents) nil nil #'equal)
           (-filter (lambda (resource-list) (equal (alist-get 'kind resource-list) "APIResourceList")))
           (-map (lambda (list) (alist-get 'resources list)))
           (-flatten-n 1)
           (-map (lambda (resource) (alist-get 'name resource)))
           (-uniq))))

(defun kele-current-context-name ()
  "Get the current context name.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (alist-get 'current-context (oref kele--global-kubeconfig-cache contents)))

(defun kele-current-namespace ()
  "Get the current context's default namespace.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (-if-let* (((&alist 'context (&alist 'namespace namespace))
              (-first (lambda (elem)
                        (string= (alist-get 'name elem) (kele-current-context-name)))
                      (alist-get 'contexts (oref kele--global-kubeconfig-cache contents)))))
      namespace))

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

(defun kele--namespaces-complete (str pred action &optional context)
  "Complete input for selection of namespaces.

STR, PRED, and ACTION are as defined in completion functions.

If CONTEXT is nil, the current context will be used."
  (if (eq action 'metadata)
      '(metadata (annotation-function . kele--namespace-annotate)
                 (category . kele-namespace))
    (complete-with-action
     action
     (kele--get-namespaces (or context (kele-current-context-name)))
     str
     pred)))

(defun kele-namespace-switch-for-context (context namespace)
  "Switch to NAMESPACE for CONTEXT."
  (interactive (let ((context (completing-read "Context: " #'kele--contexts-complete)))
                 (list context
                       (completing-read (format "Namespace (%s): " context)
                                        (lambda (str pred action)
                                          (kele--namespaces-complete str pred
                                                                     action context))))))
  (kele-kubectl-do "config" "set-context" context "--namespace" namespace))

(defun kele-namespace-switch-for-current-context (namespace)
  "Switch to NAMESPACE for the current context."
  (interactive (list (completing-read (format "Namespace (%s): " (kele-current-context-name)) #'kele--namespaces-complete)))
  (kele-kubectl-do "config" "set-context" (kele-current-context-name) "--namespace" namespace))

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
  (interactive (list (completing-read "Context to rename: " #'kele--contexts-complete)
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
    (apply #'kele--cache-namespaces context (kele--fetch-namespaces context))))

(defun kele--fetch-namespaces (context)
  "Fetch namespaces for CONTEXT."
  (-if-let* (((&alist 'port port) (kele--ensure-proxy context))
             (url (format "http://localhost:%s/api/v1/namespaces" port))
             (data (plz 'get url :as #'json-read))
             ((&alist 'items items) data))
      (-map (-lambda ((&alist 'metadata (&alist 'name name))) name)
            (append items '()))
    (signal 'error "Failed to fetch namespaces")))

(defun kele--get-cache-ttl-for-resource (resource)
  "Get the cache TTL for RESOURCE."
  (or (alist-get resource kele-resource-refresh-overrides)
      kele-resource-default-refresh-interval))

(defun kele--cache-namespaces (context &rest namespace-names)
  "Cache NAMESPACE-NAMES as the associated namespaces for CONTEXT.

The cache has a TTL as defined by
`kele-resource-refresh-overrides' and
`kele-resource-default-refresh-interval'."
  (add-to-list 'kele--context-namespaces `(,(intern context) . ,namespace-names))
  (run-with-timer
   (kele--get-cache-ttl-for-resource 'namespace)
   nil
   #'kele--clear-namespaces-for-context
   context))

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

(provide 'kele)

;;; kele.el ends here
