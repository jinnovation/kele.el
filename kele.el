;;; kele.el --- Kubernetes Enablement Layer for Emacs -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2022 Jonathan Jin

;; Author: Jonathan Jin <me@jonathanj.in>

;; Version: 0.0.1
;; Homepage: https://github.com/jinnovation/kele.el
;; Keywords: kubernetes
;; Package-Requires: ((emacs "27.1") (f "0.20.0") (yaml "0.5.1"))

;;; Commentary:

;; kele.el is a streamlines integration between Kubernetes and Emacs.  It
;; provides a "base layer" that can be leveraged to build higher-level
;; integrations, e.g. modeline modules and interactive clients.
;;
;; For more details, see: https://github.com/jinnovation/kele.el.

;;; Code:

(require 'f)
(require 'yaml)

(defgroup kele nil
  "Integration constructs for Kubernetes."
  :group 'external
  :prefix "kele-"
  :link '(url-link "https://github.com/jinnovation/kele.el"))

(defcustom kele-kubeconfig-path
  (expand-file-name (or (getenv "KUBECONFIG") "~/.kube/config"))
  "Path to the kubeconfig file."
  :type 'file)

(defvar kele-current-context nil
  "The current kubectl context.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'.")

(defvar kele-current-namespace nil
  "The current kubectl namespace.

The value is kept up-to-date with any changes to the underlying
configuration, e.g. via `kubectl config'.")

(defun kele--update (&optional _)
  "Update `kele-k8s-context' and `kele-k8s-namespace'.

Values are parsed from the contents at `kele-kubeconfig-path'."
  (when-let* ((config (yaml-parse-string (f-read kele-kubeconfig-path)))
              (current-context (ht-get config 'current-context))
              (contexts (-concat (ht-get config 'contexts) '()))
              (context (-first (lambda (elem) (string= (ht-get elem 'name) current-context)) contexts)))
    (let ((namespace (ht-get* context 'context 'namespace)))
      (setq kele-current-context current-context
            kele-current-namespace namespace))))

(defvar kele--kubeconfig-watcher nil
"Descriptor of the file watcher on `kele-kubeconfig-path'.")

(define-minor-mode kele-mode
"Minor mode to enable listening on Kubernetes configs."
:global t
:group 'kele
:lighter nil
(if (not kele-mode)
    (file-notify-rm-watch kele--kubeconfig-watcher)
  (setq kele--kubeconfig-watcher
        (file-notify-add-watch kele-kubeconfig-path '(change) 'kele--update))
  (kele--update)))

(provide 'kele)

;;; kele.el ends here
