;;;  test-kele.el --- Test Kele functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/undercover-init.el")

(require 'kele)

(describe "kele-status-simple"
  :var (kele-current-context kele-current-namespace)
  (it "renders both context and namespace correctly"
    (setq kele-current-context "foo"
          kele-current-namespace "bar")
    (expect (kele-status-simple) :to-equal "k8s:foo(bar)"))
  (it "renders correctly with no namespace"
    (setq kele-current-context "foo"
          kele-current-namespace nil)
    (expect (kele-status-simple) :to-equal "k8s:foo")))

;;; test-kele.el ends here
