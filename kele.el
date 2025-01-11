;;; kele.el --- Spritzy Kubernetes cluster management -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2025 Jonathan Jin

;; Author: Jonathan Jin <me@jonathanj.in>

;; Version: 0.4.1
;; Homepage: https://github.com/jinnovation/kele.el
;; Keywords: kubernetes tools
;; SPDX-License-Identifier: Apache-2.0
;; Package-Requires: ((emacs "29.1") (async "1.9.7") (dash "2.19.1") (f "0.20.0") (ht "2.3") memoize (plz "0.8.0") (s "1.13.0") (yaml "0.5.1"))

;;; Commentary:

;; kele.el enables nimble, lightweight management of Kubernetes clusters.
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
(require 'memoize)
(require 'plz)
(require 's)
(require 'subr-x)
(require 'transient)
(require 'treesit)
(require 'vtable)
(require 'url-parse)
(require 'yaml)

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

(defcustom kele-proxy-ttl 180
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

(defcustom kele-resource-refresh-overrides '((namespace . :never))
  "Resource-specific cache time-to-live overrides.

If a resource is listed here, the corresponding value will be
used for cache time-to-live for that resource.  Otherwise,
`kele-resource-default-refresh-interval' is used.

If the value is :never, then the resource will be cached once and
then never expired.

Keys are the singular form of the resource name, e.g. \"pod\" for
pods."
  :type '(alist :key-type symbol :value-type (radio
                                              (integer :tag "Expiration duration in seconds")
                                              (const :tag "Never expire once cached" :never)))
  :group 'kele)

(defcustom kele-discovery-refresh-interval
  600
  "Default interval for polling clusters' discovery cache."
  :type 'integer
  :group 'kele)

(defvar kele-after-context-switch-hook nil
  "Normal hook run after switching to a new context.")

(defvar kele--discovery-last-refresh-time nil
  "Timestamp of last successful poll of the discovery cache.")

(defvar kele--loggable-kinds '("pods" "deployments" "jobs" "services")
  "Resource kinds that can be passed to kubectl log.")

(defvar kele--port-forwardable-kinds '("pods" "deployments" "services")
  "Resource kinds that can be passed to kubectl port-forward.")

(defvar kele--active-port-forwards nil
  "Alist of active port-forward processes.

Each entry is a cons of local port (in string form) to (CONTEXT
NAMESPACE GVK NAME PROCESS).")

;; TODO (#80): Display in the `kele-get-mode' header what fields were filtered out
(defcustom kele-filtered-fields '((metadata managedFields)
                                  (metadata annotations kubectl.kubernetes.io/last-applied-configuration))
  "Top-level resource fields to never display, e.g. in `kele-get'."
  :type '(repeat (repeat symbol)))

(defcustom kele-confirm-deletions t
  "Whether or not to confirm before deleting resources."
  :type 'boolean
  :group 'kele)

(defcustom kele-yaml-highlighting-mode
  (cond ((featurep 'yaml-mode) 'yaml-mode)
        ((and (featurep 'yaml-ts-mode) (treesit-ready-p 'yaml)) 'yaml-ts-mode))
  "Which major mode to use for YAML highlighting.

Set to nil to disable YAML highlighting."
  :type '(choice (const nil) symbol))

(define-error 'kele-cache-lookup-error
  "Kele failed to find the requested resource in the cache.")
(define-error 'kele-request-error "Kele failed in querying the Kubernetes API")
(define-error 'kele-ambiguous-groupversion-error
  "Found multiple group-versions associated with the given resource")

(defface kele-disabled-face
  '((t (:inherit font-lock-comment-face)))
  "Face used for disabled or not-applicable values."
  :group 'kele)

(defmacro kele--with-progress (msg &rest body)
  "Execute BODY with a progress reporter using MSG.

Returns the last evaluated value of BODY."
  (declare (indent defun))
  `(let ((prog (make-progress-reporter ,msg))
         (res (progn ,@body)))
     (progress-reporter-done prog)
     res))

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

(defconst kele--random-port-range '(3000 9000))

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

(cl-defun kele-kubectl-do-sync (args &key (silent t) (suppress-error t))
  "Execute kubectl with ARGS synchronously, returning the error code.

Unless SUPPRESS-ERROR is non-nil, errors in the kubectl
invocation will signal an error in Emacs.

Unless SILENT is non-nil, will log the command output."
  (with-temp-buffer
    (let ((exit-code (apply #'call-process kele-kubectl-executable
                            nil
                            (current-buffer)
                            nil
                            "--kubeconfig" kele-kubeconfig-path
                            args)))
      (if (= 0 exit-code)
          (progn
            (unless silent (message (s-trim-right (buffer-string))))
            exit-code)
        (unless suppress-error (error (buffer-string)))
        exit-code))))

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
   (timer
    :documentation "The timer process for polling the filesystem."))
  "Track the Kubernetes discovery cache.

A class for loading a Kubernetes discovery cache and keeping it
in sync with the filesystem.")

(defclass kele--kubeconfig-cache ()
  ((contents
    :documentation "The loaded kubeconfig contents.")
   (filewatch-id
    :documentation "The ID of the file watcher.")
   (update-in-progress
    :documentation "Flag denoting whether an update is in progress."
    :initform nil))
  "Track the kubeconfig cache.

A class for loading kubeconfig contents and keeping them in sync
with the filesystem.")

(cl-defmethod kele--wait ((cache kele--kubeconfig-cache)
                          &key
                          (count 10)
                          (wait 1)
                          (timeout 100)
                          (msg "Waiting for kubeconfig update to finish..."))
  "Wait for CACHE to finish updating.

COUNT, WAIT, and TIMEOUT are as defined in `kele--retry'.

MSG is the progress reporting message to display."
  (when (oref cache update-in-progress)
    (kele--with-progress msg
      (kele--retry (lambda () (not (oref cache update-in-progress)))
                   :count count :wait wait :timeout timeout))))

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
   nil nil (-cut compare-strings <> nil nil <> nil nil t)))

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

(defun kele--resource-list-find-resource (name field resource-list)
  "Return non-nil when RESOURCE-LIST has resource with value NAME at FIELD.

RESOURCE-LIST expected to be an alist mirroring the
APIResourceList schema."
  (->> (alist-get 'resources resource-list)
       (-any (lambda (resource)
               (equal (alist-get field resource) name)))))

(cl-defmethod kele--get-discovery-resource ((cache kele--discovery-cache)
                                            type &key context (lookup-key 'name))
  "Look up the discovery cache entry in CACHE using TYPE and LOOKUP-KEY.

If CONTEXT is nil, use the current context."
  (let* ((ctx (or context (kele-current-context-name)))
         (resource-lists (kele--get-resource-lists-for-context cache ctx))

         ;; TODO:
         ;; - Accept group-version optional kwarg
         ;; - Error if len(filtered-resource-lists) > 1 AND group-version not specified
         ;; - Filter for group-version
         (filtered-resource-lists (-filter (-partial #'kele--resource-list-find-resource type lookup-key) resource-lists)))

    (when (> (length filtered-resource-lists) 1)
      (warn "More than one group-version found for resource name `%s'; assuming `%s'"
            type
            (alist-get 'groupVersion (car filtered-resource-lists))))

    (let-alist (car filtered-resource-lists)
      (let* ((resource (-first (lambda (resource)
                                 (string-equal
                                  (alist-get lookup-key resource)
                                  type))
                               .resources)))
        resource))))

(cl-defmethod kele--get-kind-for-plural ((cache kele--discovery-cache)
                                         type
                                         &key context)
  "Look up the Kind name for the given resource TYPE in CACHE.

TYPE is expected to be the plural name of the resource.

If CONTEXT is nil, use the current context."
  (alist-get 'kind (kele--get-discovery-resource cache type :context context)))

(cl-defmethod kele--get-singular-for-plural ((cache kele--discovery-cache)
                                             type
                                             &key context)
  "Look up the singular name for a given resource TYPE in CACHE.

TYPE is expected to be the plural name of the resource.

If CONTEXT is nil, use the current context."
  (let-alist (kele--get-discovery-resource cache type :context context)
    (if (or (not .singularName) (string-equal .singularName ""))
        (downcase .kind)
      .singularName)))

(cl-defmethod kele--resource-namespaced-p ((cache kele--discovery-cache)
                                           group-version
                                           type
                                           &key context)
  "Look up the namespaced-ness of GROUP-VERSION TYPE in CACHE.

If CONTEXT is not provided, the current context is used."
  (if-let* ((namespaced-p
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
                          (progress-reporter-done progress-reporter)
                          (setq kele--discovery-last-refresh-time (current-time)))))
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
  (oset cache timer
        (run-with-timer
         (if bootstrap 0 kele-discovery-refresh-interval)
         kele-discovery-refresh-interval
         (-partial #'kele--cache-update cache))))

(cl-defmethod kele--cache-stop ((cache kele--discovery-cache))
  "Stop polling for CACHE."
  (cancel-timer (oref cache timer)))

(defvar kele--enabled nil
  "Flag indicating whether Kele has already been enabled or not.

This is separate from `kele-mode' to ensure that activating
`kele-mode' is idempotent.")
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
                          (oset cache update-in-progress nil)
                          (progress-reporter-done progress-reporter))))
    (oset cache update-in-progress t)
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

(cl-defstruct (kele--gvk
               (:constructor kele--gvk-create)
               (:copier nil))
  "Group-Version-Kind object.

GROUP and VERSION are not always set.  KIND is always set."
  group
  version
  kind)

(cl-defmethod kele--gv-string ((gvk kele--gvk))
  "Return group-version string for GVK."
  (if (oref gvk group)
      (concat (oref gvk group) "/" (oref gvk version))
    (oref gvk version)))

(cl-defmethod kele--string ((gvk kele--gvk))
  "Return string representation of the full GVK."
  (let ((vk (format "%s.%s" (oref gvk version) (oref gvk kind))))
    (if (oref gvk group)
        (concat (oref gvk group) "/" vk)
      vk)))

(cl-defmethod kele--singular ((gvk kele--gvk) &optional context)
  "Return the name of GVK in singular form.

If CONTEXT is provided, look up in that context.  Otherwise, use
the current context."
  (kele--get-singular-for-plural
   kele--global-discovery-cache
   (oref gvk kind)
   :context (or context (kele-current-context-name))))

(cl-defstruct (kele--proxy-record
               (:constructor kele--proxy-record-create)
               (:copier nil))
  "Record of a proxy server process.

PROCESS is the process itself.

PORT is the port the process is running on.

TIMER, if non-nil, is the cleanup timer."
  process
  port
  timer)

(cl-defmethod kele--url ((proxy kele--proxy-record))
  (format "http://localhost:%s" (kele--proxy-record-port proxy)))

(cl-defmethod ready-p ((proxy kele--proxy-record))
  "Return non-nil if the PROXY is ready for requests.

Returns nil on any curl error."
  (let ((ready-addr (format "%s/readyz" (kele--url proxy)))
        (live-addr (format "%s/livez" (kele--url proxy))))
    (condition-case _err
        (when-let* ((resp-ready (plz 'get ready-addr :as 'response :else 'ignore))
                    (resp-live (plz 'get live-addr :as 'response :else 'ignore))
                    (status-ready (plz-response-status resp-ready))
                    (status-live (plz-response-status resp-live)))
          (and (= 200 status-ready) (= 200 status-live)))
      (error nil))))

(defclass kele--proxy-manager ()
  ((records
    :documentation
    "Alist of context names to `kele--proxy-record' objects."
    :initarg :records
    :type list
    :initform nil))
  "Manage proxy server processes.")

(cl-defmethod proxy-start ((manager kele--proxy-manager)
                           context
                           &key port (ephemeral t))
  "Start a proxy process within MANAGER for CONTEXT at PORT.

If EPHEMERAL is non-nil, the proxy process will be cleaned up
after a certain amount of time.

If PORT is nil, a random port will be chosen.

Returns the proxy process.

If CONTEXT already has a proxy process active, this function returns the
existing process *regardless of the value of PORT*."
  (-if-let ((&alist context record) (oref manager records))
      record
    (kele--with-progress (format "Starting proxy server process for `%s'..."
                                 context)
      (let* ((selected-port (or port (kele--random-port)))
             (record (kele--proxy-record-create
                      :process (kele--proxy-process context :port selected-port :wait nil)
                      :timer (when ephemeral
                               (run-with-timer kele-proxy-ttl nil (-partial #'proxy-stop
                                                                            manager
                                                                            context)))
                      :port selected-port)))
        (oset manager records (cons `(,context . ,record) (oref manager records)))
        record))))

(cl-defmethod proxy-get ((manager kele--proxy-manager)
                         context
                         &key (wait t))
  "Retrieve the proxy process from MANAGER for CONTEXT.

If WAIT is non-nil, polls the liveliness and health endpoints for
  the proxy server until they respond successfully.

This function assumes that the proxy process has already been started.  It will
  not start a proxy server if one has not already been started."
  (-when-let ((&alist context record) (oref manager records))
    (when wait
      (kele--retry (-partial #'ready-p record) :wait 2 :count 10))
    record))

(cl-defmethod proxy-stop ((manager kele--proxy-manager)
                          context)
  "Stop the proxy process in MANAGER for CONTEXT.

If no process active for CONTEXT, this function is a no-op and
returns nil."
  (-when-let ((&alist context record) (oref manager records))
    (kele--kill-process-quietly (kele--proxy-record-process record))
    (when (kele--proxy-record-timer record)
      (cancel-timer (kele--proxy-record-timer record)))
    (oset manager records (assoc-delete-all context (oref manager records))))
  (message (format "[kele] Stopped proxy for context `%s'" context)))

(cl-defmethod proxy-active-p ((manager kele--proxy-manager)
                              context)
  "Return non-nil if a proxy serve is active for CONTEXT in MANAGER."
  (cdr (assoc context (oref manager records))))

(defvar kele--global-proxy-manager (kele--proxy-manager))

;; FIXME: Returns nil sometimes...
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

(cl-defun kele-current-context-name (&key (wait t))
  "Get the current context name.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'.

If WAIT is non-nil, does not wait for any current update process
to complete.  Returned value may not be up to date."
  (when wait
    (kele--wait kele--global-kubeconfig-cache))
  (alist-get 'current-context (oref kele--global-kubeconfig-cache contents)))

(defun kele--default-namespace-for-context (context)
  "Get the default namespace for CONTEXT."
  (-if-let* (((&alist 'context (&alist 'namespace namespace))
              (-first (lambda (elem)
                        (string= (alist-get 'name elem) context))
                      (alist-get 'contexts (oref kele--global-kubeconfig-cache contents)))))
      namespace))

(cl-defun kele-current-namespace (&key (wait t))
  "Get the current context's default namespace.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'.

If WAIT is non-nil, does not wait for any current update process
to complete.  Returned value may not be up to date."
  (kele--default-namespace-for-context (kele-current-context-name :wait wait)))

(defun kele-status-simple ()
  "Return a simple status string suitable for modeline display."
  (let* ((ctx (kele-current-context-name :wait nil))
         (ns (kele-current-namespace :wait nil))
         (status (if (not (or ctx ns))
                     "--"
                   (concat ctx (if ns (concat "(" ns ")") "")))))
    (concat "k8s:" status)))

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
  (if-let* ((context (-first (lambda (elem) (string= (alist-get 'name elem) context-name))
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
         (proxy-status (if (proxy-active-p kele--global-proxy-manager context-name)
                           (propertize "Proxy ON" 'face 'warning)
                         (propertize "Proxy OFF" 'face 'shadow))))
    (format " %s%s, %s, %s%s"
            (propertize "(" 'face 'completions-annotations)
            (propertize cluster-name 'face 'completions-annotations)
            (propertize server 'face 'completions-annotations)
            proxy-status
            (propertize ")" 'face 'completions-annotations))))

(cl-defun kele--namespaces-complete (&key context prompt initial-input history)
  "Complete input for namespaces in CONTEXT using PROMPT.

If user does not have permission to list namespaces, simply
prompt user for verbatim string.

If CONTEXT is not provided, use the current context."
  (let ((ctx (or context (kele-current-context-name))))
    (completing-read
     (or prompt (format "Namespace (%s): " ctx))
     (when (kele--can-i
            :resource "namespaces"
            :group "core"
            :verb 'list
            :context ctx)
       (-cut kele--resources-complete <> <> <>
             :cands (kele--get-namespaces ctx)
             :category 'kele-namespace))
     nil t initial-input history)))

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
                 (list context (kele--namespaces-complete :context context))))
  (kele-kubectl-do "config" "set-context" context "--namespace" namespace))

(transient-define-suffix kele-namespace-switch-for-current-context (namespace)
  "Switch to NAMESPACE for the current context."
  :key "n"
  :description
  (lambda ()
    (format "Change default namespace of %s to..."
            (propertize (oref transient--prefix scope)
                        'face 'warning)))
  (interactive
   (let ((ctx (if (and transient--prefix
                      (slot-boundp transient--prefix 'scope))
                 (oref transient--prefix scope)
               (kele-current-context-name))))
     (list
      (kele--namespaces-complete :context ctx))))
  (kele-namespace-switch-for-context
   (if (and transient--prefix
            (slot-boundp transient--prefix 'scope))
       (oref transient--prefix scope)
     (kele-current-context-name))
   namespace))

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

(defun kele--contexts-read (prompt initial-input history)
  "Reader function for contexts.

PROMPT, INITIAL-INPUT, and HISTORY are all as defined in Info
node `(elisp)Programmed Completion'."
  (completing-read prompt #'kele--contexts-complete nil t initial-input history))

(transient-define-suffix kele-context-switch (context)
  "Switch to a new CONTEXT."
  :key "s"
  :description
  (lambda ()
    (format "Switch from %s to..."
            (propertize (if (and transient--prefix
                                 (slot-boundp transient--prefix 'scope))
                            (oref transient--prefix scope)
                          (kele-current-context-name))
                        'face
                        'warning)))
  (interactive (list (completing-read "Context: " #'kele--contexts-complete)))
  (kele--with-progress (format "Switching to use context `%s'..." context)
    (kele-kubectl-do "config" "use-context" context)
    (run-hooks 'kele-after-context-switch-hook)))

;; TODO(#176): Update `kele--namespace-cache'
(transient-define-suffix kele-context-rename (old-name new-name)
  "Rename context named OLD-NAME to NEW-NAME."
  :key "r"
  :description "Rename a context"
  (interactive (list (completing-read "Context to rename: "
                                      #'kele--contexts-complete
                                      nil t nil nil (kele-current-context-name))
                     (read-from-minibuffer "Rename to: ")))
  ;; TODO(#176): This needs to update `kele--global-proxy-manager' as well.
  (kele-kubectl-do "config" "rename-context" old-name new-name))

;; TODO(#176): Update `kele--namespace-cache'
(transient-define-suffix kele-context-delete (context)
  :key "d"
  :description "Delete a context"
  (interactive (list (completing-read "Context: " #'kele--contexts-complete)))
  (kele--with-progress (format "Deleting context `%s'..." context)
    (kele-kubectl-do "config" "delete-context" context)))

(cl-defun kele-proxy-stop (context)
  "Clean up the proxy for CONTEXT."
  (interactive (list (completing-read "Stop proxy for context: " #'kele--contexts-complete)))
  (proxy-stop kele--global-proxy-manager context))

(cl-defun kele-proxy-start (context &key port (ephemeral t))
  "Start a proxy process for CONTEXT at PORT.

If EPHEMERAL is non-nil, the proxy process will be cleaned up
after a certain amount of time.

If PORT is nil, a random port will be chosen.

Returns an alist with keys `proc', `timer', and `port'.

If CONTEXT already has a proxy process active, this function returns the
existing process *regardless of the value of PORT*."
  (interactive (list (completing-read "Start proxy for context: " #'kele--contexts-complete)
                     :port nil
                     :ephemeral t))
  (proxy-start kele--global-proxy-manager context :port port :ephemeral ephemeral))

(defun kele-proxy-toggle (context)
  "Start or stop proxy server process for CONTEXT."
  (interactive (list (completing-read
                      "Start/stop proxy for context: "
                      #'kele--contexts-complete)))
  (funcall
   (if (proxy-active-p kele--global-proxy-manager context)
       #'kele-proxy-stop
     #'kele-proxy-start)
   context))

(defvar kele--context-resources nil
  "An alist mapping contexts to their cached resources.

Values are: (RESOURCE-NAME . (list RESOURCE)).")

(defvar kele--namespaces-cache nil
  "An alist mapping contexts to their constituent namespaces.

If value is nil, the namespaces need to be fetched directly.")

(defun kele--clear-namespaces-for-context (context)
  "Clear the stored namespaces for CONTEXT."
  (setq kele--namespaces-cache
        (assoc-delete-all (intern context) kele--namespaces-cache)))

;; TODO: test :cache nil
(cl-defun kele--get-namespaces (context &key (cache t))
  "Get namespaces for CONTEXT.

If the namespaces are cached, return the cached value.

If CACHE is non-nil, cache the fetched namespaces."
  (if-let* ((cached-namespaces (alist-get (intern context) kele--namespaces-cache)))
      cached-namespaces
    (let* ((gvk (kele--gvk-create :version "v1" :kind "namespaces"))
           (namespaces (kele--fetch-resource-names gvk :context context)))
      (if cache
          (apply #'kele--cache-namespaces context namespaces)
        namespaces))))

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
  (add-to-list 'kele--namespaces-cache `(,(intern context) . ,namespace-names))
  (let ((ttl (kele--get-cache-ttl-for-resource 'namespace)))
    (when (and ttl (not (eq ttl :never)))
      (run-with-timer
       (kele--get-cache-ttl-for-resource 'namespace)
       nil
       #'kele--clear-namespaces-for-context
       context)))
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

(cl-defun kele--get-resource (gvk name &key context namespace)
  "Get resource GVK by NAME.

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
           ((and (oref gvk group) (oref gvk version)) (list (format "%s/%s"
                                                                    (oref gvk group)
                                                                    (oref gvk version))))
           ((and (oref gvk version) (not (oref gvk group))) (list (oref gvk version)))
           (t (kele--get-groupversions-for-type kele--global-discovery-cache
                                                (oref gvk kind)
                                                :context context)))))

    (when (> (length group-versions) 1)
      (signal 'kele-ambiguous-groupversion-error group-versions))
    (when (and namespace (not (kele--resource-namespaced-p
                               kele--global-discovery-cache
                               (car group-versions)
                               (oref gvk kind)
                               :context context)))
      (user-error "Namespace `%s' specified for un-namespaced resource `%s'; remove namespace and try again" namespace (oref gvk kind)))

    (-let* ((gv (car group-versions))
            (namespace (and (kele--resource-namespaced-p
                             kele--global-discovery-cache
                             gv
                             (oref gvk kind)
                             :context context)
                            (or namespace (kele--default-namespace-for-context context))))
            (url-gv (if (s-contains-p "/" gv)
                        (format "apis/%s" gv)
                      (format "api/%s" gv)))
            (url-res (format "%s/%s" (oref gvk kind) name))
            (url-all (concat url-gv "/"
                             (if namespace (format "namespaces/%s/" namespace) "")
                             url-res))
            (port (kele--proxy-record-port (proxy-start kele--global-proxy-manager context)))
            (url (format "http://localhost:%s/%s" port url-all)))
      (condition-case err
          (kele--resource-container-create
           :resource (kele--retry (lambda () (plz 'get url :as #'json-read)))
           :context context
           :kind (oref gvk kind)
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
    (kele-refetch . "re-fetch the resource")))

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
            (,(kbd "g") . kele-refetch))
  (when kele-get-mode
    (read-only-mode 1)))

(defun kele-refetch ()
  "Refetches the currently displayed resource."
  (interactive nil kele-get-mode)

  (-let* ((ctx kele--current-resource-buffer-context)
          ((&alist 'kind kind
                   'apiVersion api-version
                   'metadata (&alist 'name name
                                     'namespace namespace))
           (kele--resource-buffer-context-resource ctx))
          (context (kele--resource-buffer-context-context ctx))
          (gvk (kele--gvk-create
                :group (car (kele--groupversion-split api-version))
                :version (cadr (kele--groupversion-split api-version))
                :kind (kele--resource-buffer-context-kind ctx))))
    (kele--with-progress (format "Re-fetching resource `%s/%s' (namespace: %s, context: %s)..."
                                 kind
                                 name
                                 namespace
                                 context)
      (kele--render-object
       (kele--get-resource gvk name :namespace namespace :context context)
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

(defun kele--groupversion-split (group-version)
  "Split a single GROUP-VERSION string.

Returns a list where the car is the group and the cadr is the version.

Nil value for group denotes the core API."
  (let ((split (s-split "/" group-version)))
    (if (length= split 1) (list nil (car split)) split)))

(cl-defun kele--list-resources (gvk &key namespace context)
  "Return the List of the resources of type GVK.

Return value is an alist mirroring the Kubernetes List type of
the type in question.

If NAMESPACE is provided, return only resources belonging to that
namespace.  If NAMESPACE is provided for non-namespaced GVK,
throws an error.

If CONTEXT is not provided, use the current context."
  (when (and namespace
             (not (kele--resource-namespaced-p
                   kele--global-discovery-cache
                   (kele--gv-string gvk)
                   (oref gvk kind))))
    (user-error "Attempted to fetch un-namespaced resource `%s' as namespaced" (oref gvk kind)))

  (let* ((ctx (or context (kele-current-context-name)))
         (port (kele--proxy-record-port
                (proxy-start kele--global-proxy-manager ctx)))
         (url (format "http://localhost:%s/%s/%s"
                      port
                      (if (oref gvk group)
                          (format "apis/%s/%s" (oref gvk group) (oref gvk version))
                        (format "api/%s" (oref gvk version)))
                      (oref gvk kind))))
    ;; Block on proxy readiness
    (proxy-get kele--global-proxy-manager ctx :wait t)
    (if-let* ((data (kele--retry (lambda () (plz 'get url :as #'json-read)))))
        (let ((filtered-items (->> (append  (alist-get 'items data) '())
                                   (-filter (lambda (item)
                                              (if (not namespace) t
                                                (let-alist item
                                                  (equal .metadata.namespace namespace))))))))
          (setf (cdr (assoc 'items data)) filtered-items)
          data)

      (signal 'error (format "Failed to fetch %s/%s/%s" (oref gvk group) (oref gvk version) (oref gvk kind))))))

(cl-defun kele--fetch-resource-names (gvk &key namespace context)
  "Fetch names of resources belonging to GVK.

If NAMESPACE is provided, return only resources belonging to that namespace.  If
NAMESPACE is provided for non-namespaced KIND, throws an error.

If CONTEXT is not provided, use the current context."
  (let* ((resource-list (kele--list-resources
                         gvk
                        :namespace namespace
                        :context context))
         (items (append (alist-get 'items resource-list) '())))
    (-map (lambda (item) (let-alist item .metadata.name)) items)))

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
  (let* ((buf-name (concat "*kele: "
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

      (when (kele--resource-container-p object)
        (setq-local kele--current-resource-buffer-context
                    (kele--resource-buffer-context-create
                     :context (kele--resource-container-context object)
                     :kind (kele--resource-container-kind object)
                     :retrieval-time (kele--resource-container-retrieval-time object)
                     :resource (kele--resource-container-resource object)
                     :namespace (kele--resource-container-namespace object)))
        (put 'kele--current-resource-buffer-context 'permanent-local t))
      (when kele-yaml-highlighting-mode
        (funcall kele-yaml-highlighting-mode))
      (kele-get-mode 1))
    (select-window (display-buffer buf))))

(defun kele--prune (alist &rest keys)
  "Delete the sub-tree of ALIST corresponding to KEYS."
  (let ((prev)
        (curr alist))
    (dolist (key keys)
      (setq prev curr)
      (setq curr (if-let* ((list-p (listp curr))
                           (res (assq key curr)))
                     (cdr res)
                   nil)))
    (when curr
      (ignore (assq-delete-all (car (last keys)) prev)))
    alist))

(defvar kele--context-keymap nil
  "Keymap for actions on Kubernetes contexts.

Only populated if Embark is installed.")

(defvar kele--namespace-keymap nil
  "Keymap for actions on Kubernetes namespaces.

Only populated if Embark is installed.")

(defconst kele--embark-keymap-entries '((kele-context . kele--context-keymap)
                                        (kele-namespace . kele--namespace-keymap)))

(transient-define-suffix kele-find-kubeconfig ()
  "Open the configured kubeconfig file in a buffer."
  :key "c"
  :description
  (lambda ()
    (format "Open %s" (propertize kele-kubeconfig-path 'face 'transient-value)))
  (interactive)
  (if (file-exists-p kele-kubeconfig-path)
      (find-file kele-kubeconfig-path)
    (message "[kele] %s does not exist; try initializing kubectl first." kele-kubeconfig-path)))

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

(defun kele--precheck ()
  "Return an error if `kele-mode' is not ready to be enabled.

Ensures various preconditions are met,
e.g. `kele-kubectl-executable' is actually present."

  (when (not (executable-find kele-kubectl-executable))
    (error "`%s' not found on PATH" kele-kubectl-executable))

  (when (not (file-exists-p kele-kubeconfig-path))
    (error "`%s' does not exist; set up kubectl properly and try again" kele-kubeconfig-path)))

(defun kele--enable ()
  "Enables Kele functionality.

This is idempotent."
  (kele--precheck)
  (unless kele--enabled
    (setq kele--enabled t)
    ;; FIXME: Update the watcher when `kele-kubeconfig-path' changes.
    (let ((kubeconfig-future (kele--cache-start
                              kele--global-kubeconfig-cache
                              :bootstrap t)))
      ;; FIXME: Update the watcher when `kele-cache-dir' changes.
      (kele--cache-start kele--global-discovery-cache :bootstrap t)

      (advice-add #'vtable-beginning-of-table :before-until #'kele--vtable-beginning-of-table)
      (advice-add #'vtable-end-of-table :before-until #'kele--vtable-end-of-table)

      (kele--setup-embark-maybe)

      ;; menu bar update requires kubeconfig cache to be populated, so we wait
      ;; for the sync to complete here
      (async-wait kubeconfig-future)
      (add-hook 'menu-bar-update-hook 'kele--update-contexts-menu))))

(defun kele--disable ()
  "Disable Kele functionality.

This is idempotent."
  (unless (not kele--enabled)
    (setq kele--enabled nil)
    (kele--cache-stop kele--global-kubeconfig-cache)
    (kele--cache-stop kele--global-discovery-cache)

    (advice-remove #'vtable-beginning-of-table #'kele--vtable-beginning-of-table)
    (advice-remove #'vtable-end-of-table #'kele--vtable-end-of-table)

    (remove-hook 'menu-bar-update-hook 'kele--update-contexts-menu)
    (kele--teardown-embark-maybe)))

(defvar kele-mode-map (make-sparse-keymap)
  "Keymap for Kele mode.")

(defvar kele-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "c") #'kele-config)
    (define-key map (kbd "r") #'kele-resource)
    (define-key map (kbd "p") #'kele-ports)
    (define-key map (kbd "?") #'kele-dispatch)
    map)
  "Keymap for Kele commands.")

;;;###autoload
(define-minor-mode kele-mode
  "Minor mode to enable listening on Kubernetes configs."
  :global t
  :keymap kele-mode-map
  :group 'kele
  :lighter nil
  (if (not kele-mode)
      (kele--disable)
    (kele--enable)))

(defun kele--namespace-read-from-prefix (prompt initial-input history)
  "Read a namespace value using PROMPT, INITIAL-INPUT, and HISTORY.

Assumes that the current Transient prefix's :scope is an alist w/ `context' key."
  ;; FIXME: Make this resilient to the prefix's scope not having a context
  ;; value.  If not present (or the scope is not an alist or the scope is not
  ;; defined), default to current context.
  (if-let* ((context (alist-get 'context (oref transient--prefix scope))))
      (kele--namespaces-complete
       :context context
       :prompt prompt
       :initial-input initial-input
       :history history)
    (error "Unexpected nil context in `%s'" (oref transient--prefix command))))

(defclass kele--transient-scope-mutator (transient-option)
  ((fn
    :initarg :fn
    :initform (lambda (_scope _val) nil)
    :type function
    :documentation
    "The logic for updating the prefix's scope on each new value
assignment.

Defaults to a no-op."))
  "A Transient suffix that also modifies the associated prefix's
scope.")

(cl-defmethod transient-infix-set ((obj kele--transient-scope-mutator) new-value)
  "Set the infix NEW-VALUE while modifying the current prefix's scope.

On each invocation, calls OBJ's `fn' field with the prefix's
scope and NEW-VALUE."
  (cl-call-next-method obj new-value)
  (when (slot-boundp obj 'fn)
    (funcall (oref obj fn) (oref transient--prefix scope) new-value)))

(defclass kele--transient-infix-resetter (transient-option)
  ((resettees
    :initarg :resettees
    :initform nil
    :type list
    :documentation
    "List of arguments on the same prefix to reset when this one changes."))
  "A Transient infix that can also \"reset\" any of its peer infixes.")

(cl-defmethod transient-infix-set ((obj kele--transient-infix-resetter) val)
  "Set the infix VAL for OBJ.

Also resets any specified peer arguments on the same prefix that
  match any element of `:resettees' on OBJ."
  (cl-call-next-method obj val)
  (dolist (arg (oref obj resettees))
    (when-let* ((obj (cl-find-if (lambda (obj)
                                   (and (slot-boundp obj 'argument)
                                        (equal (oref obj argument) arg)))
                                 transient--suffixes)))
      (transient-init-value obj))))

(defclass kele--transient-infix (kele--transient-infix-resetter
                                 kele--transient-scope-mutator)
  ())

(defclass kele--transient-switches (transient-infix)
  ;; NB(@jinnovation): At some point we might need to expand this class to
  ;; include the "no value is a value" case, but that's not today. :D
  ((options
   :initarg :options
   :initform (lambda () nil)
   :type function
   :documentation
   "Function that returns the options for this infix to cycle through.

The car will be the default value.")))

(cl-defmethod transient-infix-read ((obj kele--transient-switches))
  "Read the new selected value for OBJ.

This does not prompt for user input.  Instead, it cycles through
  the CHOICES on invocation by the user."
  (let ((choices (oref obj choices))
        (value (oref obj value)))
    (or (cadr (member value choices))
        (car choices))))

(cl-defmethod transient-init-value ((obj kele--transient-switches))
  "Initialize the selected value for OBJ.

This is the car of the OPTIONS function's retval."
  (oset obj choices (funcall (oref obj options)))
  (oset obj value (car (oref obj choices))))

(cl-defmethod transient-format-value ((obj kele--transient-switches))
  "Formats the selection options for OBJ.

This draws heavy inspiration from `transient-switches', with the
following differences:

1. The ARGUMENT slot is not a part of the formatted value;

2. We do not allow non-selection as an option."
  (with-slots (value choices) obj
    (mapconcat
     (lambda (choice)
       (propertize choice 'face
                   (if (equal choice value)
                       'transient-value
                     'transient-inactive-value)))
     choices
     (propertize "|" 'face 'transient-inactive-value))))

;; FIXME: This is kind of contrived; could we find a way not to need to set
;; --argument here when we literally don't use it other than as a glorified
;; index?
(cl-defmethod transient-infix-value ((obj kele--transient-switches))
  "Return the value of OBJ.

This concatenates the `argument' slot with the `value' slot so
  that the value can be retrieved via `transient-arg-value'."
  (concat (oref obj argument) (oref obj value)))

(cl-defmethod transient-prompt ((_ kele--transient-switches))
  "Return a nil prompt.

`kele--transient-switches' does not prompt for user input."
  nil)

(transient-define-infix kele--namespace-infix ()
  "Select a namespace to work with.

Defaults to the default namespace for the currently active
context as set in `kele-kubeconfig-path'."
  :prompt "Namespace: "
  :description "namespace"
  :key "=n"
  :argument "--namespace="
  :class 'transient-option
  :reader 'kele--namespace-read-from-prefix
  :if
  (lambda ()
    (-let (((&alist 'group-versions gvs 'kind kind)
            (oref transient--prefix scope)))
      (kele--resource-namespaced-p kele--global-discovery-cache (car gvs) kind)))
  :init-value (lambda (obj)
                (oset obj value
                      (kele--default-namespace-for-context
                       (alist-get 'context (oref transient--prefix scope))))))

(transient-define-infix kele--context-infix ()
  "Select a Kubernetes context to execute a given command in.

Defaults to the currently active context as set in
`kele-kubeconfig-path'."
  :class 'kele--transient-infix
  :fn (lambda (scope value)
        (proxy-start kele--global-proxy-manager value)
        (setf (cdr (assoc 'context scope)) value))
  :resettees '("--namespace=")
  :prompt "Context: "
  :description "context"
  :key "=c"
  :argument "--context="
  :always-read t
  :reader 'kele--contexts-read
  :init-value (lambda (obj)
                (oset obj value (kele-current-context-name))))

(transient-define-infix kele--groupversions-infix ()
  :class 'kele--transient-switches
  :key "v"
  :description "group-version"
  :argument "--groupversion="
  :options (lambda ()
             (alist-get 'group-versions (oref transient--prefix scope))))

(cl-defun kele--get-context-arg ()
  "Get the value to use for Kubernetes context.

First checks the current Transient command's arguments if set.
Otherwise, returns the current context name from kubeconfig."
  (if transient-current-command
      (transient-arg-value "--context=" (transient-args
                                         transient-current-command))
    (kele-current-context-name)))

(cl-defun kele--get-namespace-arg (&key (permit-nil nil) use-default group-version kind (prompt "Namespace: "))
  "Get the value to use for Kubernetes namespace.

In order of priority, this function attempts the following:

- If we are currently in a Transient command; if so, pull the
  `--namespace=`' argument from it.  If PERMIT-NIL is non-nil,
  return this value unconditionally;

- If USE-DEFAULT is non-nil, use the default namespace for the context argument
  (from `kele--get-context-arg');

- If the resource type specified by GROUP-VERSION and KIND is not
  namespaced, return nil;

- Otherwise, ask the user to select a namespace using PROMPT."
  (let ((transient-arg-maybe (->> transient-current-command
                                  (transient-args)
                                  (transient-arg-value "--namespace="))))
    (cond
     ((or transient-arg-maybe permit-nil) transient-arg-maybe)
     ((or (not (and group-version kind)) use-default)
      (kele--default-namespace-for-context (kele--get-context-arg)))
     ((not (kele--resource-namespaced-p
            kele--global-discovery-cache
            group-version
            kind))
      nil)
     (t (completing-read prompt
                         (kele--get-namespaces (kele--get-context-arg)))))))

(cl-defun kele--get-groupversion-arg (&optional kind)
  "Get the group-version to use for a command.

First checks the current Transient command's arguments if set.
Otherwise, prompts user to select from the group-versions
available for the argument KIND.  If there is only one, no user
prompting and the function simply returns the single option."
  (if-let* ((cmd transient-current-command)
            (args (transient-args cmd))
            (value (transient-arg-value "--groupversion=" args)))
      value
    (let ((gvs (kele--get-groupversions-for-type
                kele--global-discovery-cache
                kind
                :context (kele--get-context-arg))))
      (if (= (length gvs) 1)
          (car gvs)
        (completing-read (format "Desired group-version of `%s': "
                                 kind)
                         gvs)))))

(defun kele--get-gvk-arg ()
  "Get the GVK to use for a command."
  (-let* ((kind (kele--get-kind-arg))
          (gv (kele--get-groupversion-arg kind))
          ((group version) (kele--groupversion-split gv)))
    (kele--gvk-create :group group :version version :kind kind)))


(defvar kele--list-context nil
  "The context corresponding to the current `kele-list-mode' buffer.")

(defvar kele--list-gvk nil
  "The group-version-kind corresponding to the current `kele-list-mode' buffer.")

(defun kele--list-get-at-point ()
  "`kele-get's the current object at point."
  (let-alist (vtable-current-object)
    (kele-get kele--list-context .metadata.namespace kele--list-gvk .metadata.name)))

(defun kele-list-table-dwim ()
  "Run the default action on `kele-list' table entries.

If the cursor is over an Owner cell, `kele-get' the first owning
resource.

Otherwise, simply `kele-get' the resource at point."
  (interactive nil kele-list-mode)
  (let* ((tbl (vtable-current-table))
        (col (vtable-current-column))
        (colname (vtable-column tbl col)))
    (if (string-equal colname "Owner(s)")
        (let-alist (vtable-current-object)
          ;; TODO: Implement selection for multi-owner
          (cond ((>= (length .metadata.ownerReferences) 1)
                 (kele-get
                  kele--list-context
                  .metadata.namespace
                  (kele--gvk-create :kind
                                    (alist-get 'name
                                               (kele--get-discovery-resource
                                                kele--global-discovery-cache
                                                (alist-get 'kind (elt .metadata.ownerReferences 0))
                                                :lookup-key
                                                'kind)))
                  (alist-get 'name (elt .metadata.ownerReferences 0))))
                (t (kele--list-get-at-point))))
      (kele--list-get-at-point))))

(defun kele-list-kill ()
  "Delete the resource at the current line."
  (interactive nil kele-list-mode)
  (let-alist (vtable-current-object)
    (kele-delete
     kele--list-context
     .metadata.namespace
     (kele--gv-string kele--list-gvk)
     (oref kele--list-gvk kind)
     .metadata.name))
  (vtable-revert-command))

(defvar kele-list-table-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'kele-list-table-dwim)
    (define-key map (kbd "k") #'kele-list-kill)
    map))

(defvar kele-list-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "g") #'kele-list-refresh)
    map))

;; TODO: Let each entry also optionally contain the column spec, e.g. :width and
;; :align parameters
(defvar kele--list-columns
  '((nil . (("Name" . (lambda (r)
                        (let-alist r .metadata.name)))
            ("Namespace" . (lambda (r) (let-alist r
                                         (if (not .metadata.namespace)
                                             (propertize "N/A" 'face 'kele-disabled-face)
                                           .metadata.namespace))))
            ("Created" . (lambda (r) (let-alist r .metadata.creationTimestamp)))
            ("Owner(s)" . (lambda (r)
                            (let-alist r
                              (if (not .metadata.ownerReferences)
                                  (propertize "N/A" 'face 'kele-disabled-face)
                                (if (> (length .metadata.ownerReferences) 1)
                                    "Multiple"
                                  (let-alist (elt .metadata.ownerReferences 0)
                                    (format "%s/%s" .kind .name)))))))))
    ;; NB(@jinnovation): A namespace can be in one of two phaces: Active or
    ;; Terminating.
    ;;
    ;; See: https://github.com/kubernetes/design-proposals-archive/blob/main/architecture/namespaces.md#phases
    (namespaces . (("Phase" . (lambda (r)
                                 (let-alist r
                                   ;; TODO: On cursor hover, display info about
                                   ;; the implications of the phase. For
                                   ;; example, for Terminating, explain that no
                                   ;; add'l resources can be created in that
                                   ;; namespace during that time.
                                   ;;
                                   ;; Maybe a good use case for Eldoc?
                                   (propertize .status.phase 'face
                                               (cond ((string-equal
                                                       .status.phase "Active")
                                                      'success)
                                                     ((string-equal
                                                       .status.phase
                                                       "Terminating")
                                                      'warning)
                                                     (t 'default))))))))
    (secrets . (("Type" . (lambda (r) (alist-get 'type r)))))
    (configmaps . (("Data" . (lambda (r)
                               (length (map-keys (alist-get 'data r)))))))
    (pods . (("Ready" . (lambda (r)
                          (let-alist r (format "%s/%s"
                                               (->> .status.containerStatuses
                                                    (-map (lambda (status)
                                                            (alist-get 'ready status)))
                                                    (-non-nil)
                                                    (length))
                                               (length .status.containerStatuses)))))
             ("Status" . (lambda (r)
                           (let-alist r .status.phase)))
             ("Restarts" . (lambda (r)
                             (let-alist r
                               (->> .status.containerStatuses
                                    (-map (lambda (status)
                                            (alist-get 'restartCount status)))
                                    (-sum)))))))
    (deployments . (("Ready" . (lambda (r)
                                 (let-alist r (format "%s/%s" .status.readyReplicas .status.replicas))))
                    ("Up-to-date" . (lambda (r)
                                      (let-alist r .status.updatedReplicas)))
                    ("Available" . (lambda (r) (let-alist r
                                                 .status.readyReplicas))))))
  "Alist containing column specifications for `kele-list'.

Keys are symbols representing the plural form of Kubernetes
resource kinds, e.g. `deployments'.  Values are alists mapping
column names to unary functions that take the resource object and
return the corresponding value.

The nil key contains columns general to all resources.")

(defun kele--make-list-vtable (gvk context namespace)
  "Construct an interactive vtable listing resources of GVK.

CONTEXT and NAMESPACE are according to Kubernetes conventions and
serve to further specify the resources to list.

If NAMESPACE is nil, displays resources for all namespaces."
  (let ((columns
         (append
          ;; FIXME: Pull these from the nil-key entries in kele--list-columns
          '((:name "Name" :width 30 :align left)
            (:name "Namespace" :width 20 :align left)
            (:name "GVK" :width 10 :align left)
            (:name "Owner(s)" :width 20 :align left)
            (:name "Created" :width 30 :align left))
          (mapcar (lambda (key)
                    `(:name ,key :width ,(length key) :align left))
                  (map-keys (alist-get (intern (oref gvk kind)) kele--list-columns))))))
    (make-vtable
     :insert nil
     :use-header-line nil
     :objects-function
     (lambda ()
       (let ((resource-list (kele--list-resources
                             gvk
                             :context context
                             :namespace namespace)))
         (alist-get 'items resource-list)))
     :sort-by '((0 ascend) (1 ascend))
     :columns columns
     :keymap kele-list-table-map
     :getter
     (lambda (object column vtable)
       (when-let* ((column-specs (append
                                  `(("GVK" . (lambda (_) (kele--string ,gvk))))
                                  (alist-get nil kele--list-columns)
                                  (alist-get (intern (oref gvk kind)) kele--list-columns)))
                   (colname (vtable-column vtable column))
                   (f (alist-get colname column-specs nil nil #'string-equal)))
         (funcall f object))))))

(define-derived-mode kele-list-mode fundamental-mode "Kele: List"
  "Major mode for listing multiple resources of a single kind."
  :group 'kele
  :interactive nil
  (read-only-mode 1))

(defun kele--vtable-beginning-of-table ()
  "Backport of `vtable-beginning-of-table' from Emacs HEAD.

See bug#58712.  Remove when Emacs 30 is released."
  (when (eq major-mode #'kele-list-mode)
    (if (or (text-property-search-backward 'vtable (vtable-current-table) #'eq)
            (get-text-property (point) 'vtable))
        (point)
      (goto-char (point-min)))))

(defun kele--vtable-end-of-table ()
  "Backport of `vtable-end-of-table' from Emacs HEAD.

See bug#58712.  Remove when Emacs 30 is released."
  (when (eq major-mode #'kele-list-mode)
    (if (text-property-search-forward 'vtable (vtable-current-table) #'eq)
        (point)
      (goto-char (point-max)))))

(defun kele-list-refresh ()
  "Refresh the `kele-list-mode' buffer."
  (interactive nil kele-list-mode)
  (kele--with-progress "[kele] Updating list buffer"
    (save-excursion
      (point-min)
      (while-let ((match (text-property-search-forward 'vtable)))
        (goto-char (prop-match-beginning match))
        (vtable-revert-command)
        (goto-char (prop-match-end match))))))

(transient-define-suffix kele-list (group-version kind context namespace)
  "List all resources of a given GROUP-VERSION and KIND.

If CONTEXT is provided, use it.  Otherwise, use the current context as reported
by `kele-current-context-name'.

If NAMESPACE is provided, use it.  A nil value for NAMESPACE
fetches across all namespaces.  If NAMESPACE is provided and the
KIND is not namespaced, returns an error."
  :key "l"
  :inapt-if-not
  ;; TODO(#185): Make this account for group + version as well
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (kele--can-i :verb 'list :resource .kind :context .context)))
  :description
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (if (kele--can-i :verb 'list :resource .kind :context .context)
          (format "List all %s" (propertize .kind 'face 'warning))
        (format "Don't have permission to list %s" .kind))))
  (interactive
   (-let* ((kind (kele--get-kind-arg))
           (group-version (kele--get-groupversion-arg kind)))
     (list group-version
           kind
           (kele--get-context-arg)
           (when (kele--resource-namespaced-p kele--global-discovery-cache
                                              group-version
                                              kind)
             (kele--get-namespace-arg :permit-nil t)))))
  (-let* (((group version) (kele--groupversion-split group-version))
          (gvk (kele--gvk-create :group group :version version :kind kind))
          (buf (get-buffer-create (format "*kele: %s/%s [%s(%s)]*"
                                          group-version
                                          kind
                                          context
                                          (or namespace "<all namespaces>")))))
    (with-current-buffer buf
      (setq-local kele--list-context context)
      (setq-local kele--list-gvk gvk)
      (put 'kele--list-context 'permanent-local t)
      (put 'kele--list-gvk 'permanent-local t)

      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert "Context: " context "\n")
        (insert "\n")
        (vtable-insert (kele--make-list-vtable gvk context namespace)))
      (kele-list-mode))
    (select-window (display-buffer buf))))

(cl-defun kele--get-kind-arg ()
  "Get the kind to work with.

First checks if the kind is set in the current Transient prefix,
if it's set.  Otherwise, prompts user for input."
  (or (and transient-current-prefix
           (alist-get 'kind (oref transient-current-prefix scope)))
      (completing-read (format
                        "Choose a kind to work with (context: `%s'): "
                        (kele--get-context-arg))
                       (kele--get-resource-types-for-context
                        (kele--get-context-arg)))))

(transient-define-suffix kele-delete (context namespace _group-version kind name)
  "Delete resource KIND named NAME.

GROUP-VERSION, NAMESPACE, KIND, and CONTEXT are all used to identify the
resource type to query for.

KIND should be the plural form of the kind's name, e.g. \"pods\"
instead of \"pod.\""
  :key "d"
  :inapt-if-not
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (kele--can-i :verb 'delete :resource .kind :context .context)))
  :description
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (if (kele--can-i
           :verb 'delete
           :resource .kind
           :context .context)
          (format "Delete a single %s" (propertize
                                        (kele--get-singular-for-plural
                                         kele--global-discovery-cache
                                         .kind
                                         :context .context)
                                        'face
                                        'warning))
        (format "Don't have permission to delete %s" .kind))))
  (interactive
   (-let* ((kind (kele--get-kind-arg))
           (gv (kele--get-groupversion-arg kind))
           ((group version) (kele--groupversion-split gv))
           (ns (kele--get-namespace-arg
                :group-version gv
                :kind kind
                :use-default nil))
           (cands (kele--fetch-resource-names (kele--gvk-create
                                               :group group
                                               :version version
                                               :kind kind)
                                              :namespace ns
                                              :context (kele--get-context-arg)))
           (name (completing-read "Name: " (-cut kele--resources-complete <> <>
                                                 <> :cands cands))))
     (list (kele--get-context-arg) ns gv kind name)))
  (if (or (not kele-confirm-deletions)
          (yes-or-no-p
           (format "Delete %s/%s in %s (context %s)?"
                   kind name namespace context)))
      (kele-kubectl-do-sync `("delete"
                              "--namespace" ,namespace
                              "--context" ,context
                              ,kind
                              ,name)
                            :silent nil
                            :suppress-error nil)
    (message "Aborted deletion.")))

(transient-define-suffix kele-get (context namespace gvk name)
  "Get resource GVK by NAME and display it in a buffer.

NAMESPACE and CONTEXT are all used to identify the resource type
to query for."
  :key "g"
  ;; TODO(#185): Make this account for group + version as well
  :inapt-if-not
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (kele--can-i :verb 'get :resource .kind :context .context)))
  :description
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (if (kele--can-i :verb 'get :resource .kind :context .context)
          (format "Get a single %s"
                  (propertize
                   (kele--get-singular-for-plural kele--global-discovery-cache .kind :context .context)
                   'face
                   'warning))
        (format "Don't have permission to get %s" .kind))))
  (interactive
   (-let* ((gvk (kele--get-gvk-arg))
           (ns (kele--get-namespace-arg
                :group-version (kele--gv-string gvk)
                :kind (oref gvk kind)
                :permit-nil t
                :use-default nil))
           (cands (kele--fetch-resource-names gvk :namespace ns :context (kele--get-context-arg)))
           (name (completing-read "Name: " (-cut kele--resources-complete <> <> <> :cands cands))))
     (list (kele--get-context-arg) ns gvk name)))
  (kele--render-object (kele--get-resource gvk name :namespace namespace :context context)))

(transient-define-suffix kele-port-forward (context namespace gvk name port)
  "Create a port-forward for resource GVK named NAME at PORT.

NAMESPACE and CONTEXT are used to identify the resource type to query for."
  :key "F"
  :description
  "Port-forward to..."
  :if
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (-contains? kele--port-forwardable-kinds .kind)))
  (interactive
   (let* ((gvk (kele--get-gvk-arg))
          (context (kele--get-context-arg))
          (ns (kele--get-namespace-arg
               :kind (oref gvk kind)
               :group-version (kele--gv-string gvk)
               :use-default nil))
          (cands (kele--fetch-resource-names gvk :namespace ns :context context))
          (name (completing-read
                 (format "%s to port-forward to: " (kele--singular gvk))
                 (-cut kele--resources-complete <> <> <> :cands cands)))

          (resource (kele--get-resource gvk name :context context :namespace ns))

          ;; TODO: Error if the port is already in use
          ;; TODO: Extend port completion to all appropriate resource types
          (port (if (string-equal (oref resource kind) "services")
                    (completing-read "Port: "
                                     (-map (lambda (port-spec) (number-to-string (alist-get 'port port-spec)))
                                           (kele--service-ports resource))
                                     nil t)
                  (number-to-string (read-number "Port: ")))))
     (list context ns gvk name port)))
  (let* ((proc-name (format "kele: port-forward (%s/%s, %s, %s, %s)" (oref gvk kind) name context namespace port))
         (proc (make-process
                :name proc-name
                :command (list kele-kubectl-executable
                               "--context"
                               context
                               "--namespace"
                               namespace
                               "port-forward"
                               (format "%s/%s" (oref gvk kind) name)
                               port)
                :buffer (generate-new-buffer (format " *%s*" proc-name))
                :sentinel
                (lambda (proc _status)
                  (let ((exit-code (process-exit-status proc)))
                    (cl-case exit-code
                      (0 (message "Successfully terminated port-forward for %s" name))
                      (9 (message "Port-forward for %s (port %s) terminated" name port))
                      (t (message "Port-forward process for %s failed with exit code %s" name exit-code)))
                    (kele--kill-process-quietly proc)))
                :noquery t)))
    (add-to-list 'kele--active-port-forwards (list port context namespace gvk name proc))
    proc)
  (message "[kele] Started port-forward for %s/%s (port %s)" (oref gvk kind) name port))

(defun kele--port-forward-affixation (ports)
  "Affixation function for port-forwards.

PORTS is used according to `completion-extra-properties'."
  (mapcar (lambda (port)
            (let ((record (alist-get port kele--active-port-forwards nil nil #'equal)))
              (list port
                    (propertize
                     (format "%s/%s:"
                             (oref (car (nthcdr 2 record)) kind)
                             (car (nthcdr 3 record)))
                     'face 'completions-annotations)
                    (propertize
                     (format " (context: %s, namespace: %s)"
                             (car (nthcdr 0 record))
                             (car (nthcdr 1 record)))
                     'face 'completions-annotations))))
          ports))

(cl-defun kele--service-ports (obj &key (protocol nil))
  "Get the exposed ports for service OBJ.

If PROTOCOL is provided, filter for only ports of that protocol.

OBJ is assumed to be a `kele--resource-container' containing a
Service resource."
  (let-alist (oref obj resource)
    (let ((ports .spec.ports))
      (if protocol
          (-filter (lambda (port-spec)
                     (equal
                      (alist-get 'protocol port-spec)
                      protocol))
                   (append ports '()))
        (append ports '())))))

(defun kele--port-forwards-active-p ()
  "Return non-nil if there are any port-forwards active."
  (< 0 (length (mapcar 'car kele--active-port-forwards))))

(transient-define-suffix kele-kill-port-forward (port)
  "Kill a port-forward process.

The port-forward must have been initiated with
`kele-port-forward'."
  :description "Kill a port-forward"
  :inapt-if-not #'kele--port-forwards-active-p
  (interactive
   (if (not (kele--port-forwards-active-p))
       (error "[kele] No port-forwards active!")
    (let* ((completion-extra-properties
            (list :affixation-function #'kele--port-forward-affixation))
           (port (completing-read "Port-forward to kill: "
                                  (mapcar 'car kele--active-port-forwards))))
      (list port))))

  (let* ((record (alist-get port kele--active-port-forwards nil nil #'equal))
         (proc (car (last record))))
    (setq kele--active-port-forwards (assoc-delete-all port kele--active-port-forwards #'equal))
    (kele--kill-process-quietly proc)
    (message "Killed port-forward for port %s" port)))

(transient-define-suffix kele-deployment-restart (context namespace deployment-name)
  "Restart DEPLOYMENT-NAME.

CONTEXT and NAMESPACE are used to identify where the deployment lives."
  :key "R"
  :description "Restart a deployment"
  :if
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (string-equal "deployments" .kind)))
  (interactive
   (let* ((gvk (kele--gvk-create :kind "deployments" :group "apps" :version "v1"))
          (context (kele--get-context-arg))
          (ns (kele--get-namespace-arg
               :kind (oref gvk kind)
               :group-version (kele--gv-string gvk)
               :use-default nil))
          (cands (kele--fetch-resource-names gvk :namespace ns :context context))
          (name (completing-read "Deployment to restart: "
                                 (-cut kele--resources-complete <> <> <> :cands cands))))
     (list context ns name)))
  ;; TODO: Ask user for confirmation?
  (kele-kubectl-do-sync `("rollout"
                          "restart"
                          ,(format "deployment/%s" deployment-name)
                          ,(format "--context=%s" context)
                          ,(format "--namespace=%s" namespace))
                        :silent nil
                        :suppress-error nil))

(transient-define-prefix kele-resource (group-versions kind)
  "Work with Kubernetes resources."
  ["Arguments"
   (kele--context-infix)
   (kele--groupversions-infix)
   (kele--namespace-infix)]

  [["General Actions"
   (kele-get)
   (kele-delete)
   (kele-list)]

   [:description
    (lambda ()
      (string-join
       (list (--> (oref transient--prefix scope)
                  (alist-get 'kind it)
                  (kele--get-kind-for-plural kele--global-discovery-cache it)
                  (propertize it 'face 'warning))
             (propertize "-specific actions" 'face 'transient-heading))))
    (kele-resource-follow-logs)
    (kele-port-forward)
    (kele-deployment-restart)]]

  (interactive (let* ((context (kele-current-context-name))
                      (kind (completing-read
                             (format "Choose a kind to work with (context: `%s'): " context)
                             (kele--get-resource-types-for-context
                              context)))
                      (gvs (kele--get-groupversions-for-type
                            kele--global-discovery-cache
                            kind
                            :context context)))
                 (list gvs kind)))
  (proxy-start kele--global-proxy-manager (kele-current-context-name))
  ;; TODO: Can we make proxy-start non-ephemeral and simply terminate the proxy
  ;; server when the transient exits?
  (transient-setup 'kele-resource nil nil :scope `((group-versions . ,group-versions)
                                                   (kind . ,kind)
                                                   (context . ,(kele-current-context-name)))))

;; TODO:
;; - `kele-log-mode-map' with bindings to change parameters and reload the buffer

;; FIXME: Known issues:
;; - Might not support multi-container pods
(transient-define-suffix kele-resource-follow-logs (context namespace gvk name)
  :key "L"
  :if
  (lambda ()
    (let-alist (oref transient--prefix scope)
      (-contains? kele--loggable-kinds .kind)))
  :description "Follow logs"
  (interactive
   (-let* ((gvk (kele--get-gvk-arg))
           (ns (kele--get-namespace-arg
                :group-version (kele--gv-string gvk)
                :kind (oref gvk kind)
                :use-default nil))
           (cands (kele--fetch-resource-names gvk :namespace ns :context (kele--get-context-arg)))
           (name (completing-read "Name: " (-cut kele--resources-complete <> <> <> :cands cands))))
     (list (kele--get-context-arg) ns gvk name)))
  (let* ((buf-name (format "*kele: logs: %s/%s*" (kele--string gvk) name))
         (name-func (lambda (_) buf-name))
         (cmd (format "kubectl logs --follow --context %s --namespace %s %s/%s"
                      context
                      namespace
                      (kele--gvk-kind gvk)
                      name)))
    (compilation-start cmd t name-func)))

(transient-define-prefix kele-dispatch ()
  "Work with Kubernetes clusters and configs."
  ["Work with..."
   ("c" "Configurations" kele-config)
   ("r" "Resources" kele-resource)
   ("p" "Proxy servers" kele-proxy)])

(transient-define-suffix kele--toggle-proxy-current-context (context)
  :key "p"
  :description
  (lambda ()
    (format "%s proxy server for %s"
            (if (proxy-active-p
                 kele--global-proxy-manager
                 (oref transient--prefix scope))
                "Disable"
              "Enable")
            (propertize (oref transient--prefix scope) 'face
                        'warning)))
  (interactive (list (oref transient-current-prefix scope)))
  (kele-proxy-toggle context))

(transient-define-prefix kele-config ()
  "Work with Kubernetes configurations.

The `scope' is the current context name."
  [[:description
    (lambda ()
      (format "Context: %s"
              (propertize (oref transient--prefix scope) 'face 'warning)))
    (kele-context-switch)
    (kele-namespace-switch-for-current-context)
    (kele--toggle-proxy-current-context)]
   ["Actions"
   (kele-context-rename)
   (kele-context-delete)]
   ["Files"
    (kele-find-kubeconfig)]]
  (interactive)
  (transient-setup 'kele-config nil nil :scope (kele-current-context-name)))

(transient-define-prefix kele-ports ()
  "Work with ports in Kubernetes."
  [["Proxy servers"
    (kele--toggle-proxy-current-context)
    ("P" kele-proxy-toggle :description "Start/stop proxy server for...")]
   ["Ports"
    ("k" kele-kill-port-forward)]]
  (interactive)
  (transient-setup 'kele-ports nil nil :scope (kele-current-context-name)))

(easy-menu-define kele-menu-map kele-mode-map
  "Menu for Kubernetes management.

Similar to `kele-dispatch'."
   '("Kubernetes"
     ("Configuration"

      ;; placeholder for dynamic fill-in (see `kele--update-contexts-menu')
      ["Switch context to..."
       nil
       :help "Waiting for kubeconfig sync to complete..."
       :enable nil]
      ["Switch namespace for current context to..."
       nil
       :help "Waiting for kubeconfig sync to complete..."
       :enable nil]
      "---"
      ["Find config file"
       kele-find-kubeconfig
       :help "Open the active kubeconfig file in a buffer"])))

(defun kele--update-contexts-menu ()
  "Fill in the context-switch sub-menu with candidate contexts."
  (let ((ctx-current (kele-current-context-name :wait nil))
        (ns-current (kele-current-namespace :wait nil)))
    (easy-menu-add-item
     kele-menu-map
     '("Configuration")
     ;; FIXME: If user doesn't have list-namespace auth, fallback to option that
     ;; simply asks user for verbatim namespace name
     (append '("Switch namespace for current context to...")
             (mapcar (lambda (ns)
                       (vector ns
                               (lambda ()
                                 (interactive)
                                 (kele-namespace-switch-for-context ctx-current ns))
                               :help (format "Switch to namespace `%s' for context `%s'" ns ctx-current)
                               :style 'radio
                               :enable (not (string-equal ns ns-current))
                               :selected (string-equal ns ns-current)))
                     (kele--get-namespaces ctx-current))))
    (easy-menu-add-item
     kele-menu-map
     '("Configuration")
     (append '("Switch context to...")
             (mapcar (lambda (ctx)
                       (vector ctx
                               (lambda ()
                                 (interactive)
                                 (kele-context-switch ctx))
                               :help (format "Switch to context `%s'" ctx)
                               :style 'radio
                               :enable (not (string-equal ctx ctx-current))
                               :selected (string-equal ctx ctx-current)))
                     (kele-context-names))))))

(cl-defun kele--mk-self-subject-access-review
    (&key resource
          (group "*")
          (verb 'get)
          (version "*"))
  "Stub out a SelfSubjectAccessReview for GROUP, RESOURCE, and VERB.

Return the resulting SelfSubjectAccessReview in alist form."
  `((apiVersion . "authorization.k8s.io/v1")
    (kind . "SelfSubjectAccessReview")
    (spec . ((resourceAttributes . ((group . ,group)
                                    (resource . ,resource)
                                    (version . ,version)
                                    (verb . ,(symbol-name verb))))))))

(cl-defun kele--can-i (&key resource group (verb 'get) context)
  "Return whether or not user can perform VERB on RESOURCE in GROUP.

If CONTEXT is not provided, uses current context."
  (let* ((ctx (or context (kele-current-context-name)))
         (port (kele--proxy-record-port (proxy-start kele--global-proxy-manager
                                                     ctx)))
         (url (string-join (list (format "http://localhost:%s" port)
                                 "apis"
                                 "authorization.k8s.io"
                                 "v1"
                                 "selfsubjectaccessreviews")
                           "/")))
    ;; Block on proxy readiness
    (proxy-get kele--global-proxy-manager ctx :wait t)
    (--> (plz
           'post
           url
           :headers '(("Content-Type" . "application/json"))
           :body (json-encode (kele--mk-self-subject-access-review
                               :resource resource
                               :group group
                               :verb verb))
           :as #'json-read)
         (-let (((&alist 'status (&alist 'allowed allowed)) it))
           allowed))))

;; Memoize can-i so that we can use auth info in high-"refresh" settings
;; e.g. menu bar redrawing without having to create and block on proxy server
;; every single time
(memoize 'kele--can-i)

;; TODO: Method to invalidate all caches (manual as well as memoized)

(provide 'kele)

;;; kele.el ends here
