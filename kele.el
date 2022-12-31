;;; kele.el --- Interface with Kubernetes -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022 Jonathan Jin

;; Author: Jonathan Jin <me@jonathanj.in>

;; Version: 0.0.1
;; Homepage: https://github.com/jinnovation/kele.el
;; Keywords: kubernetes tools
;; Package-Requires: ((emacs "27.1") (dash "2.19.1") (f "0.20.0") (ht "2.3") (yaml "0.5.1"))

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

(cl-defun kele-kubectl-do (&rest args)
  "Execute kubectl with ARGS."
  (let ((cmd (append (list kele-kubectl-executable) args)))
    (make-process
     :name (format "kele: %s" (s-join " " cmd))
     :command cmd
     :noquery t)))



(defvar kele-current-namespace nil
  "The current kubectl namespace.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'.")

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
  "Update `kele-k8s-context' and `kele-k8s-namespace'.

Values are parsed from the contents at `kele-kubeconfig-path'."
  (when-let* ((config (kele--get-config))
              (contexts (-concat (ht-get config 'contexts) '())))
    (setq kele--config config)
    (when-let ((current-context (ht-get config 'current-context)))
      (let* ((context (-first (lambda (elem) (string= (ht-get elem 'name) current-context)) contexts))
             (namespace (ht-get* context 'context 'namespace)))
        (setq kele-current-namespace namespace)))))

(defun kele-current-context-name ()
  "Get the current context name.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'."
  (ht-get kele--config 'current-context))

(defun kele-status-simple ()
  "Return a simple status string suitable for modeline display."
  (let ((status (if (not (or (kele-current-context-name) kele-current-namespace))
                    "--"
                  (concat (kele-current-context-name)
                          (if kele-current-namespace
                              (concat "(" kele-current-namespace ")")
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
