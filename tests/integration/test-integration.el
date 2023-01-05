(load-file "./tests/integration/undercover-init.el")

(require 'async)
(require 'f)
(require 'kele)
(require 'plz)

(describe "config interactions"
  (describe "kele-namespace-switch-for-context"
    (it "switches context properly"
      (kele-context-switch "kind-kele-test-cluster0")
      (kele-namespace-switch-for-context "kind-kele-test-cluster0" "kube-public")
      (async-wait (kele--update-kubeconfig))
      (expect (kele-current-namespace) :to-equal "kube-public"))))

(describe "kele--fetch-namespaces"
  (it "fetches namespace names"
    (expect (kele--fetch-namespaces "kind-kele-test-cluster0")
            :to-have-same-items-as
            '("default"
              "kube-node-lease"
              "kube-public"
              "kube-system"
              "local-path-storage"))))

(describe "kele--proxy-process"
  (it "successfully creates a proxy process"
    (kele--proxy-process "kind-kele-test-cluster0" :port 9999 :wait t :read-only t)
    (expect (plz-response-body (kele--retry (lambda () (plz 'get "http://127.0.0.1:9999/readyz" :as 'response)))) :to-equal "ok")))
