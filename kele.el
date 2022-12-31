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

(cl-defun kele--retry (fn &key (count 5) (wait 1) (timeout 100))
  "Retry FN COUNT times, waiting WAIT seconds between each.

Returns unconditionally after TIMEOUT seconds.

Returns the retval of FN."
  (let ((retval))
    (with-timeout (timeout)
      (let ((_count count))
        (while (and (> _count 0) (not (setq retval (funcall fn))))
          (setq _count (- _count 1))
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
  (-if-let* ((updated-plist
              (plist-put (plist-put body :sync t)
                         ;; Suppress the default request.el error handler; we
                         ;; check the error later
                         :error (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                               nil))))
             (resp (apply #'request url updated-plist))
             (err (request-response-error-thrown resp)))
      (if fatal (signal 'error (list (cdr err))) nil)
    resp))

(cl-defun kele-proxy-process (context port &key (wait t) (read-only t))
  "Create a new kubectl proxy process for CONTEXT.

The proxy will be opened at PORT (localhost:PORT).  It is the
  caller's responsibility to ensure that the port is not
  occupied.

If READ-ONLY is set, the proxy will only accept read-only
requests.

If WAIT is non-nil, `kele-proxy-process' will wait for the proxy
  to be ready before returning.  This wait is a best effort; the
  proxy's /livez and /readyz endpoints are not guaranteed to
  return 200s by end of wait."
  (let* ((s-port (number-to-string port))
         (proc-name (format "kele: proxy (%s, %s)" context port))
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
                (lambda (proc status)
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
  (yaml-parse-string (f-read kele-kubeconfig-path)))

;; TODO: Can this be done async?
(defun kele--update (&optional _)
  "Update `kele--config'.

Values are parsed from the contents at `kele-kubeconfig-path'."
  (setq kele--config (kele--get-config)))

(defun kele-current-context-name ()
  "Get the current context name.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (ht-get kele--config 'current-context))

(defun kele-current-namespace ()
  "Get the current context's default namespace.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (if-let* ((context (-first
                      (lambda (elem)
                        (string= (ht-get elem 'name) (kele-current-context-name)))
                      (kele-contexts))))
      (ht-get* context 'context 'namespace)))

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
  (-map (lambda (elem) (ht-get elem 'name)) (kele-contexts)))

(defun kele-contexts ()
  "Get all contexts.

Each element is a hash-table representing the entry in
kubeconfig."
  (-concat (ht-get kele--config 'contexts) '()))

(defun kele--context-cluster (context-name)
  "Get the cluster of the context named CONTEXT-NAME."
  (if-let ((context (-first (lambda (elem) (string= (ht-get elem 'name) context-name))
                            (kele-contexts))))
      (ht-get* context 'context 'cluster)
    (error "Could not find context of name %s" context-name)))

(defun kele--context-annotate (context-name)
  "Return annotation text for the context named CONTEXT-NAME."
  (let* ((context (-first (lambda (elem)
                           (string= (ht-get elem 'name) context-name))
                          (kele-contexts)))
        (cluster-name (ht-get* context 'context 'cluster))
        (cluster (-first (lambda (elem)
                           (string= (ht-get elem 'name) cluster-name))
                         (-concat (ht-get kele--config 'clusters) '())))
        (server (ht-get* cluster 'cluster 'server)))
    (s-concat " (" cluster-name ", " server ")")))

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
  (kele-kubectl-do "config" "rename-context" old-name new-name))

(defvar kele--context-keymap nil
  "Keymap for actions on Kubernetes contexts.

Only populated if Embark is installed.")
(defconst kele--embark-keymap-entries '((kele-context . kele--context-keymap)))

(defun kele--setup-embark-maybe ()
  "Optionally set up Embark integration."
  (when (featurep 'embark)
    (with-suppressed-warnings ((free-vars embark-keymap-alist embark-general-map))
      (setq kele--context-keymap (let ((map (make-sparse-keymap)))
                                   (define-key map "s" #'kele-context-switch)
                                   (define-key map "r" #'kele-context-rename)
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
