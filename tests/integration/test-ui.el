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
      (spy-on 'kele-get)

      (async-wait (kele--cache-update kele--global-discovery-cache))
      (async-wait (kele--cache-update kele--global-kubeconfig-cache)))

    (describe "prefix buffer contents"
      (before-each
        (with-simulated-input
         "deployments RET"
         (call-interactively #'kele-resource)))

      (it "sets the current context as default value"
        (with-current-buffer transient--buffer-name
          (expect (buffer-string) :to-match "--context=kind-kele-test-cluster0")))
      (it "sets the current context's default namespace as the default value"
        (expect (buffer-string) :to-match "--namespace=kube-public")))

    (describe "get"
      (it "retrieves with the appropriate parameters"
        (with-simulated-input
         "deployments RET g coredns RET"
         (expect 'kele-get :to-have-been-called-with
                 "deployments" "coredns"
                 :group "apps"
                 :version "v1"
                 :namespace "kube-public"
                 :context "kind-kele-test-cluster0"))))))

;;; test-ui.el ends here

