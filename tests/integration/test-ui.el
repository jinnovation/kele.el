;;; test-ui.el --- Test UI-based functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(load-file "./tests/integration/undercover-init.el")

(require 'async)
(require 'dash)
(require 'with-simulated-input)

(require 'kele)

(describe "Transient prefixes"
  (describe "kele-resource"

    (before-each
      (async-wait (kele--cache-update kele--global-discovery-cache))
      (async-wait (kele--cache-update kele--global-kubeconfig-cache))
      (with-simulated-input
       "deployments RET"
       (call-interactively #'kele-resource)))

    (it "sets the current context as default value"
      (with-current-buffer transient--buffer-name
        (expect (buffer-string) :to-match "--context=kind-kele-test-cluster0")))))

;;; test-ui.el ends here

