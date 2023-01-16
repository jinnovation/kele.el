(load-file "./tests/integration/undercover-init.el")

(require 'async)
(require 'f)
(require 'kele)
(require 'plz)
(require 'with-simulated-input)

(describe "config interactions"
  (describe "kele-namespace-switch-for-context"
    (it "switches context properly"
      (kele-context-switch "kind-kele-test-cluster0")
      (kele-namespace-switch-for-context "kind-kele-test-cluster0" "kube-public")
      (async-wait (kele--cache-update kele--global-kubeconfig-cache))
      (expect (kele-current-namespace) :to-equal "kube-public"))))

(describe "kele--fetch-resource-names"
  (it "fetches core API names"
    (expect (kele--fetch-resource-names nil "v1" "namespaces" :context "kind-kele-test-cluster0")
            :to-have-same-items-as
            '("default"
              "kube-node-lease"
              "kube-public"
              "kube-system"
              "local-path-storage")))
  (it "fetches group API names"
    (expect (kele--fetch-resource-names "apps" "v1" "deployments" :context "kind-kele-test-cluster0")
            :to-have-same-items-as
            '("coredns" "local-path-provisioner"))))

(describe "kele--get-resource"
  :var (retval)

  (it "retrieves the resource as an alist"
    (async-wait (kele--cache-update kele--global-discovery-cache))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))
    (setq retval (kele--get-resource "deployments" "coredns"
                                                :group "apps"
                                                :version "v1"
                                                :context "kind-kele-test-cluster0"
                                                :namespace "kube-system"))

    (expect (kele--resource-container-p retval) :to-be-truthy)
    (expect (let-alist (kele--resource-container-resource retval) .metadata.name) :to-equal "coredns"))

  (it "returns an error if the resource is nonsense or does not exist"
    (async-wait (kele--cache-update kele--global-discovery-cache))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))
    (expect (kele--get-resource "salaries" "mine"
                                           :group "hello"
                                           :version "v1"
                                           :context "kind-kele-test-cluster0"
                                           :namespace "kube-system")
            :to-throw 'kele-cache-lookup-error)))

(describe "kele--proxy-process"
  (it "successfully creates a proxy process"
    (kele--proxy-process "kind-kele-test-cluster0" :port 9999 :wait t :read-only t)
    (expect (plz-response-status (kele--retry (lambda () (plz 'get "http://127.0.0.1:9999/readyz" :as 'response)))) :to-equal 200)))

(describe "kele-get"
  (it "fetches the resource"

    (async-wait (kele--cache-update kele--global-discovery-cache))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))
    (with-simulated-input
        "deployments RET kube-system RET coredns RET"
      (call-interactively #'kele-get))
    (expect (-map #'buffer-name (buffer-list))
            :to-contain
            " *kele: kind-kele-test-cluster0(kube-system): Deployment/coredns*")))
