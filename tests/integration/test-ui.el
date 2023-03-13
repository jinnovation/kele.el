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

    (before-all
      (async-wait (kele--cache-update kele--global-discovery-cache))
      (async-wait (kele--cache-update kele--global-kache)))

    (describe "prefix buffer contents"
      (before-all
        (with-simulated-input
         "deployments RET"
         (call-interactively #'kele-resource)))
      (after-all
        (transient-quit-all))

      (it "sets the current context as default value"
        (with-current-buffer transient--buffer-name
          (expect (buffer-string) :to-match "--context=kind-kele-test-cluster0")))
      (it "sets the current context's default namespace as the default value"
        (with-current-buffer transient--buffer-name
          (expect (buffer-string) :to-match "--namespace=kube-public"))))

    ;; TODO: This is blocked on some odd behavior when calling Transient from
    ;; batch mode. See: magit/transient#180
    (xdescribe "get"
      (it "retrieves with the appropriate parameters"
        (spy-on 'kele-get)

        (with-simulated-input
         "deployments RET g SPC coredns RET"
         (call-interactively #'kele-resource))

         (expect 'kele-get :to-have-been-called-with
                 "deployments" "coredns"
                 :group "apps"
                 :version "v1"
                 :namespace "kube-public"
                 :context "kind-kele-test-cluster0")))))

;;; test-ui.el ends here

