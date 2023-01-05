(load-file "./tests/integration/undercover-init.el")

(require 'async)
(require 'f)
(require 'kele)
(require 'buttercup)

(describe "config interactions"
  (describe "kele-namespace-switch-for-context"
    (it "switches context properly"
      (kele-context-switch "kind-kele-test-cluster0")
      (kele-namespace-switch-for-context "kind-kele-test-cluster0" "kube-public")
      (async-wait (kele--update-kubeconfig))
      (expect (kele-current-namespace) :to-equal "kube-public"))))
