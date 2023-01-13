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
      (async-wait (kele--cache-update kele--global-kubeconfig-cache))
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

(describe "kele--get-namespaced-resource"
  :var (retval)
  (it "retrieves the resource as an alist"
    (setq retval (kele--get-namespaced-resource "deployments" "coredns"
                                                :group "apps"
                                                :version "v1"
                                                :context "kind-kele-test-cluster0"
                                                :namespace "kube-system"))

    (expect (kele--resource-container-p retval) :to-be-truthy)
    (expect (let-alist (kele--resource-container-resource retval) .metadata.name) :to-equal "coredns"))

  (it "returns an error if the resource is nonsense or does not exist"
    (expect (kele--get-namespaced-resource "salaries" "mine"
                                           :group "hello"
                                           :version "v1"
                                           :context "kind-kele-test-cluster0"
                                           :namespace "kube-system")
            :to-throw 'kele-request-error))
  (describe "when GROUP and VERSION not specified"
    (describe "when only one group-version exists for the argument resource type"
      (it "auto-selects group-version"
        (async-wait (kele--cache-update kele--global-discovery-cache))
        (async-wait (kele--cache-update kele--global-kubeconfig-cache))
        (setq retval (kele--get-namespaced-resource "deployments" "coredns"))
        (expect (let-alist (kele--resource-container-resource retval) .metadata.name) :to-equal "coredns")
        (expect (alist-get 'apiVersion (kele--resource-container-resource retval)) :to-equal "apps/v1")))

    ;; TODO: Revive some of the multiple-groupversion tests from #54
    (describe "when multiple group-versions exist for the same resource type"
      ;; NB: This is actually a unit test but we're keeping here to co-locate w/
      ;; the rest of get-namespaced-resource tests
      (before-each
        (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
        (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
        (async-wait (kele--cache-update kele--global-discovery-cache))
        (async-wait (kele--cache-update kele--global-kubeconfig-cache)))

      ;; This would allow consumers of this API to decide if they'd like to
      ;; disambiguate on users' behalf, present a completion buffer to users to
      ;; select, etc.
      (it "errors with the group-version options attached to the error"
        (expect (kele--get-namespaced-resource "ambiguousthings" "fake-name")
                :to-throw 'kele-ambiguous-groupversion-error)
        (condition-case err
            (kele--get-namespaced-resource "ambiguousthings" "fake-name")
          (kele-ambiguous-groupversion-error
           (expect (cdr err) :to-have-same-items-as '("fake-group/v1"
                                                      "fake-other-group/v1")))
          (:success (buttercup-fail "Received unexpected success")))))))

(describe "kele--proxy-process"
  (it "successfully creates a proxy process"
    (kele--proxy-process "kind-kele-test-cluster0" :port 9999 :wait t :read-only t)
    (expect (plz-response-status (kele--retry (lambda () (plz 'get "http://127.0.0.1:9999/readyz" :as 'response)))) :to-equal 200)))
