;;; kele.el --- Interface with Kubernetes -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022 Jonathan Jin

;; Author: Jonathan Jin <me@jonathanj.in>

;; Version: 0.0.1
;; Homepage: https://github.com/jinnovation/kele.el
;; Keywords: kubernetes tools
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (f "0.20.0") (ht "2.3") (request "0.3.2") (yaml "0.5.1"))

;;; Commentary:

;; kele.el streamlines integration with Kubernetes.  It provides a "base layer"
;; that can be leveraged to build higher-level integrations, e.g. modeline
;; modules and interactive clients.
;;
;; For more details, see: https://github.com/jinnovation/kele.el.

;;; Code:

(require 'dash)
(require 'f)
(require 'filenotify)
(require 'ht)
(require 'request)
(require 'subr-x)
(require 'yaml)

(defgroup kele nil
  "Integration constructs for Kubernetes."
  :group 'external
  :prefix "kele-"
  :link '(url-link "https://github.com/jinnovation/kele.el"))

(defcustom kele-kubeconfig-path
  (expand-file-name (or (getenv "KUBECONFIG") "~/.kube/config"))
  "Path to the kubeconfig file."
  :type 'file
  :group 'kele)

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

(defun kele--request-option (url fatal &rest body)
  "Send request to URL using BODY, returning error or the response.

If FATAL is nil, instead of an error, simply return nil.

This function injects :sync t into BODY."
  (if-let* ((updated-plist
             (plist-put (plist-put body :sync t)
                        ;; Suppress the default request.el error handler; we
                        ;; check the error later
                        :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                              nil))))
            (resp (apply #'request url updated-plist))
            (err (request-response-error-thrown resp)))
      (if fatal (signal 'error (list (cdr err))) nil)
    resp))

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
                     (and (= 200 (request-response-status-code (kele--request-option ready-addr nil)))
                          (= 200 (request-response-status-code (kele--request-option live-addr nil)))))))
    proc))

(cl-defun kele-kubectl-do (&rest args)
  "Execute kubectl with ARGS."
  (let ((cmd (append (list kele-kubectl-executable) args)))
    (make-process
     :name (format "kele: %s" (s-join " " cmd))
     :command cmd
     :noquery t)))

(defvar kele--kubeconfig-watcher nil
  "Descriptor of the file watcher on `kele-kubeconfig-path'.")

(defvar kele--config nil
  "The current kubeconfig.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'.")

(defun kele--get-config ()
  "Get the config at `kele-kubeconfig-path'.

The config will be represented as a hash table."
  (yaml-parse-string (f-read kele-kubeconfig-path)
                     :object-type 'alist
                     :sequence-type 'list))

;; TODO: Can this be done async?
(defun kele--update (&optional _)
  "Update `kele--config'.

Values are parsed from the contents at `kele-kubeconfig-path'."
  (setq kele--config (kele--get-config)))

(defun kele-current-context-name ()
  "Get the current context name.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (alist-get 'current-context kele--config))

(defun kele-current-namespace ()
  "Get the current context's default namespace.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (-if-let* (((&alist 'context (&alist 'namespace namespace))
              (-first (lambda (elem)
                        (string= (alist-get 'name elem) (kele-current-context-name)))
                      (alist-get 'contexts kele--config))))
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
  (-map (lambda (elem) (alist-get 'name elem)) (alist-get 'contexts kele--config)))

(defun kele--context-cluster (context-name)
  "Get the cluster of the context named CONTEXT-NAME."
  (if-let ((context (-first (lambda (elem) (string= (alist-get 'name elem) context-name))
                            (alist-get 'contexts kele--config))))
      (alist-get 'cluster (alist-get 'context context))
    (error "Could not find context of name %s" context-name)))

(defun kele--context-annotate (context-name)
  "Return annotation text for the context named CONTEXT-NAME."
  (let* ((context (-first (lambda (elem)
                            (string= (alist-get 'name elem) context-name))
                          (alist-get 'contexts kele--config)))
         (cluster-name (alist-get 'cluster (alist-get 'context context)))
         (cluster (-first (lambda (elem)
                            (string= (alist-get 'name elem) cluster-name))
                          (-concat (alist-get 'clusters kele--config) '())))
         (server (alist-get 'server (alist-get 'cluster cluster))))
    ;; TODO: Show proxy status
    (s-concat " (" cluster-name ", " server ")")))

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

(defun kele-namespace-switch (namespace &optional context)
  "Switch to NAMESPACE for CONTEXT."
  (interactive
   (let ((context (kele-current-context-name)))
     (list
      (completing-read (format "Namespace (%s): " context) #'kele--namespaces-complete)
      context)))
  (kele-kubectl-do "config" "set-context" "--current" "--namespace" namespace))

(defun kele--namespace-annotate (namespace-name)
  "Return annotation text for the namespace named NAMESPACE-NAME."
  "")

(defun kele--contexts-complete (str pred action)
  "Complete input for selection of contexts.

STR, PRED, and ACTION are as defined in completion functions."
  (if (eq action 'metadata)
      '(metadata (annotation-function . kele--context-annotate)
                 (category . kele-context))
    (complete-with-action action (kele-context-names) str pred)))

(defun kele-context-switch (context)
  "Switch to CONTEXT."
  (interactive (list (completing-read "Context: " #'kele--contexts-complete)))
  ;; TODO: Message that this has been done
  (kele-kubectl-do "config" "use-context" context))

(defun kele-context-rename (old-name new-name)
  "Rename context named OLD-NAME to NEW-NAME."
  (interactive (list (completing-read "Context to rename: " #'kele--contexts-complete)
                     (read-from-minibuffer "Rename to: ")))
  ;; TODO: This needs to update `kele--context-proxy-ledger' as well.
  (kele-kubectl-do "config" "rename-context" old-name new-name))

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

(cl-defun kele--cleanup-proxy-for-context (context)
  "Clean up the proxy for CONTEXT."
  (-let (((&alist 'proc proc 'timer timer) (alist-get (intern context) kele--context-proxy-ledger)))
    (kele--kill-process-quietly proc)
    (when timer (cancel-timer timer)))
  (setq kele--context-proxy-ledger (assoc-delete-all (intern context) kele--context-proxy-ledger)))

(cl-defun kele--start-proxy (context &key port (ephemeral t))
  "Start a proxy process for CONTEXT at PORT.

If EPHEMERAL is non-nil, the proxy process will be cleaned up
after a certain amount of time.

If PORT is nil, a random port will be chosen.

Returns the proxy process."
  (let* ((selected-port (or port (kele--random-port)))
         (key (intern context))
         (proc (kele--proxy-process context :port selected-port))
         (cleanup (when ephemeral
                    (run-with-timer kele-proxy-ttl nil #'kele--cleanup-proxy-for-context context)))
         (entry `((proc . ,proc)
                  (timer . ,cleanup)
                  (port . ,selected-port))))
    (add-to-list 'kele--context-proxy-ledger `(,key . ,entry))
    entry))

(cl-defun kele--ensure-proxy (context)
  "Return a proxy process for CONTEXT, creating one if needed."
  (if-let* ((entry (alist-get (intern context) kele--context-proxy-ledger)))
      entry
    (kele--start-proxy context)))

;; TODO: Might be worth an EIEIO class for this
(defvar kele--context-resource-names nil
  "An alist mapping contexts to their cached resources and object names.

Values are: (RESOURCE-NAME . (list OBJECT-NAME)).

All resources detected for the given context will have entries,
but not all will have cached object names.  A nil value under a
given resource denotes that the object set has not been cached; a
special value of :empty denotes that the retrieved set is simply
empty.")

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
             (resp (kele--request-option url t))
             (data (json-parse-string (request-response-data resp)))
             ((&hash "items" items) data))
      (-map (-lambda ((&hash "metadata" (&hash "name" name))) name)
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
                                   (make-composed-keymap map embark-general-map)))
      (setq kele--namespace-keymap (let ((map (make-sparse-keymap)))
                                     (define-key map "s" #'kele-namespace-switch)
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
  (setq kele--kubeconfig-watcher
        ;; FIXME: Update the watcher when `kele-kubeconfig-path' changes.
        (file-notify-add-watch kele-kubeconfig-path '(change) #'kele--update))
  (kele--setup-embark-maybe)
  (if (featurep 'awesome-tray)
      (with-suppressed-warnings ((free-vars awesome-tray-module-alist))
        (add-to-list 'awesome-tray-module-alist kele--awesome-tray-module)))
  (kele--update))

(defun kele--disable ()
  "Disable Kele functionality."
  (file-notify-rm-watch kele--kubeconfig-watcher)
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
