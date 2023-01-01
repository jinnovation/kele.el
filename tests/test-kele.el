;;;  test-kele.el --- Test Kele functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'f)
(require 'request)

(require 'kele)

(describe "kele-status-simple"
  (it "renders with context and namespace"
    (spy-on 'kele-current-context-name :and-return-value "foo")
    (spy-on 'kele-current-namespace :and-return-value "bar")
    (expect (kele-status-simple) :to-equal "k8s:foo(bar)"))
  (it "renders with no namespace"
    (spy-on 'kele-current-context-name :and-return-value "foo")
    (spy-on 'kele-current-namespace :and-return-value nil)
    (expect (kele-status-simple) :to-equal "k8s:foo"))
  (it "renders with neither namespace nor context"
    (spy-on 'kele-current-context-name :and-return-value nil)
    (spy-on 'kele-current-namespace :and-return-value nil)
    (expect (kele-status-simple) :to-equal "k8s:--")))

(describe "kele--context-annotate"
  (it "returns the proper annotation text"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (kele--update)
    (expect (kele--context-annotate "development") :to-equal " (development-cluster, https://development.org/server)")))

(describe "kele-context-names"
  (it "returns the correct cluster names"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (kele--update)

    (expect (kele-context-names) :to-equal '("development"))))

(describe "kele-context-rename"
  (before-each
    (spy-on 'kele-kubectl-do))
  (it "calls kubectl properly"
    (kele-context-rename "foo" "bar")
    (expect 'kele-kubectl-do
            :to-have-been-called-with
            "config" "rename-context" "foo" "bar")))

(describe "kele--context-cluster"
  (it "returns the correct cluster"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (kele--update)
    (expect (kele--context-cluster "development") :to-equal "development-cluster")))

(describe "kele--request-option"
  (describe "when request throws an error"
    (before-each
      (spy-on 'request
              :and-call-fake (lambda (&rest _)
                               (make-request-response
                                :status-code 404
                                :error-thrown (error . ("bar"))))))
    (it "throws an error"
      (expect (kele--request-option "foo" t) :to-throw 'error)))
  (describe "when request returns successfully"
    (before-each
      (spy-on 'request
              :and-return-value (make-request-response
                                 :status-code 200
                                 :error-thrown nil)))
    (it "returns the response"
      (expect (kele--request-option "foo" t) :to-be-truthy))))

;;; test-kele.el ends here
