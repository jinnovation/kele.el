;;;  test-kele.el --- Test Kele functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/unit/undercover-init.el")

(require 'async)
(require 'dash)
(require 'f)

(require 'kele)

(describe "kele--with-progress"
  (it "returns the retval of the last evaluated sexp"
    (expect (kele--with-progress "foobar" (= 1 1)) :to-equal t)))

(describe "kele--proxy-enabled-p"
  (before-each
    (setq kele--context-proxy-ledger '((foo . bar)
                                       (baz . nil))))

  (it "evals to non-nil if context present in `kele--context-proxy-ledger'"
    (expect (kele--proxy-enabled-p "foo") :to-be-truthy))

  (it "evals to nil if context not present in `kele--context-proxy-ledger'"
    (expect (kele--proxy-enabled-p "qux") :not :to-be-truthy)
    (expect (kele--proxy-enabled-p "baz") :not :to-be-truthy)))

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

(describe "kele-current-context-name"
  (it "returns the correct current-context value"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))
    (expect (kele-current-context-name) :to-equal "development")))

(describe "kele--context-annotate"
  (it "returns the proper annotation text"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))
    (expect (kele--context-annotate "development") :to-equal " (development-cluster, https://123.456.789.0, Proxy OFF)")))

(describe "kele-context-names"
  (it "returns the correct cluster names"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))

    (expect (kele-context-names) :to-equal '("development" "no-namespace"))))

(describe "kele-context-rename"
  (before-each
    (spy-on 'kele-kubectl-do))
  (it "calls kubectl properly"
    (kele-context-rename "foo" "bar")
    (expect 'kele-kubectl-do
            :to-have-been-called-with
            "config" "rename-context" "foo" "bar")))

(describe "kele-current-namespace"
  (before-each
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache)))
  (it "returns the default namespace for the current cluster"
    (spy-on 'kele-current-context-name :and-return-value "development")
    (expect (kele-current-namespace) :to-equal "development-namespace"))
  (it "returns nil if no default namespace"
    (spy-on 'kele-current-context-name :and-return-value "no-namespace")
    (expect (kele-current-namespace) :to-equal nil)))

(describe "kele--context-cluster-name"
  (it "returns the correct cluster"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))
    (expect (kele--context-cluster-name "development") :to-equal "development-cluster")))

(describe "kele--retry"
  :var (foo)
  (before-each
    (setf (symbol-function 'foo) (lambda () nil))
    (spy-on 'foo :and-call-through))

  (it "retries the correct number of times"
    (kele--retry 'foo :count 5 :wait 0)
    (expect 'foo :to-have-been-called-times 5)))

(describe "kele-proxy-start"
  (before-each
    (spy-on 'kele--proxy-process :and-return-value 'fake-proc)
    (spy-on 'run-with-timer :and-return-value 'fake-timer)
    (setq kele--context-proxy-ledger nil))

  (describe "when ephemeral is nil"
    (it "adds an entry with no timer"
      (expect (kele-proxy-start "foobar" :port 9999 :ephemeral nil)
              :to-equal
              '((proc . fake-proc)
                (timer . nil)
                (port . 9999)))
      (expect (alist-get 'foobar kele--context-proxy-ledger)
              :to-equal
              '((proc . fake-proc)
                (timer . nil)
                (port . 9999)))))

  (describe "when ephemeral is non-nil"
    (it "adds an entry with a timer"
      (kele-proxy-start "foobar" :port 9999)
      (expect (alist-get 'foobar kele--context-proxy-ledger)
              :to-equal
              '((proc . fake-proc)
                (timer . fake-timer)
                (port . 9999))))))

(describe "kele--ensure-proxy"
  (before-each
    (spy-on 'kele-proxy-start)
    (setq kele--context-proxy-ledger nil))
  (describe "when proxy present"
    (before-each
      (add-to-list 'kele--context-proxy-ledger '(foobar . ((proc . fake-proc)
                                                           (timer . fake-timer)
                                                           (port . 9999)))))
    (it "returns the ledger entry"
      (expect (kele--ensure-proxy "foobar") :to-equal '((proc . fake-proc)
                                                        (timer . fake-timer)
                                                        (port . 9999)))))
  (describe "when proxy not already present"
    (it "creates the proxy"
      (kele--ensure-proxy "foobar")
      (expect 'kele-proxy-start :to-have-been-called-with "foobar"))))

(describe "kele--clear-namespaces-for-context"
  (before-each
    (setq kele--context-namespaces '((foo . ("n1" "n2" "n3")))))
  (describe "when context is present in cache"
    (it "deletes the entry"
      (kele--clear-namespaces-for-context "foo")
      (expect kele--context-namespaces :to-equal nil))))

(describe "kele--cache-namespaces"
  (before-each
    (spy-on 'run-with-timer)
    (setq kele--context-namespaces nil))

  (it "adds namespaces correctly"
    (kele--cache-namespaces "foobar" "n0" "n1" "n2")
    (expect (alist-get 'foobar kele--context-namespaces)
            :to-equal
            '("n0" "n1" "n2"))
    (expect 'run-with-timer :to-have-been-called)))

(describe "resource caching"
  (before-each
    (setq kele-resource-default-refresh-interval 60)
    (setq kele-resource-refresh-overrides '((foo . 999))))

  (describe "when a resource has a TTL override"
    (it "uses the override value"
      (expect (kele--get-cache-ttl-for-resource 'foo) :to-equal 999)))
  (describe "when a resource has no TTL override"
    (it "uses the default value"
      (expect (kele--get-cache-ttl-for-resource 'bar) :to-equal 60))))

(describe "kele--cache-update (kele--discovery-cache)"
  (describe "the retval"
    (it "is keyed on host"
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update kele--global-discovery-cache))

      (expect (map-keys (oref kele--global-discovery-cache contents)) :to-have-same-items-as '("123.456.789.0")))

    (it "contains the expected resources"
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update kele--global-discovery-cache))
      (let* ((api-resource-lists (alist-get "123.456.789.0" (oref kele--global-discovery-cache contents) nil nil #'equal))
             (resource-lists (-map (lambda (arl) (alist-get 'resources arl))
                                   api-resource-lists))
             (resources (-flatten-n 1 resource-lists))
             (names (-map (lambda (r) (alist-get 'name r)) resources)))
        ;; TODO: Test for
        ;;   - num of APIResourceLists present
        ;;   - groupversions assoc'd w/ each APIResourcelist are as expected
        (expect names :to-have-same-items-as
                '("componentstatuses"
                  "configmaps"
                  "namespaces"
                  "namespaces/finalize"
                  "namespaces/status"
                  "nodes"
                  "nodes/status"
                  "pods"
                  "pods/attach"
                  "pods/binding"
                  "pods/eviction"
                  "pods/exec"
                  "pods/log"
                  "pods/portforward"
                  "pods/proxy"
                  "pods/status"
                  "replicationcontrollers"
                  "replicationcontrollers/scale"
                  "replicationcontrollers/status"
                  "resourcequotas"
                  "resourcequotas/status"
                  "secrets"
                  "serviceaccounts"
                  "services"
                  "services/proxy"
                  "services/status"
                  "ambiguousthings")
                )))))

(describe "kele--get-resource-types-for-context"
  (it "contains the expected resources"
    (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
    (async-wait (kele--cache-update kele--global-discovery-cache))

    (let ((res (kele--get-resource-types-for-context "development")))
      (expect res
              :to-have-same-items-as
              '("componentstatuses"
                "configmaps"
                "namespaces"
                "namespaces/finalize"
                "namespaces/status"
                "nodes"
                "nodes/status"
                "pods"
                "pods/attach"
                "pods/binding"
                "pods/eviction"
                "pods/exec"
                "pods/log"
                "pods/portforward"
                "pods/proxy"
                "pods/status"
                "replicationcontrollers"
                "replicationcontrollers/scale"
                "replicationcontrollers/status"
                "resourcequotas"
                "resourcequotas/status"
                "secrets"
                "serviceaccounts"
                "services"
                "services/proxy"
                "services/status"
                "ambiguousthings")))))

(describe "kele--kubeconfig-cache"
  :var (cache)
  (describe "kele--cache-start"
    (before-each
      (setq cache (kele--kubeconfig-cache))
      (spy-on 'file-notify-add-watch :and-return-value :fnotify-id)
      (spy-on 'kele--cache-update)
      (kele--cache-start cache :bootstrap t))
    (describe "when :bootstrap t"
      (it "performs an initial update"
        (expect 'kele--cache-update :to-have-been-called-with cache)))
    (it "sets the filewatch field"
      (expect (oref cache filewatch-id) :to-equal :fnotify-id))))

(describe "kele--discovery-cache"
  :var (cache)
  (describe "kele--cache-start"
    (before-each
      (setq cache (kele--discovery-cache))
      (spy-on 'kele--fnr-add-watch :and-return-value :fnotify-id)
      (spy-on 'kele--cache-update)
      (kele--cache-start cache :bootstrap t))

    (describe "when :bootstrap t"
      (it "performs an initial update"
        (expect 'kele--cache-update :to-have-been-called-with cache)))
    (it "sets the filewatch field"
      (expect (oref cache filewatch-id) :to-equal :fnotify-id)))
  (describe "kele--get-groupversions-for-type"
    (before-each
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update kele--global-discovery-cache)))
    (it "fetches all possible groupversions for the argument type"
      (expect (kele--get-groupversions-for-type kele--global-discovery-cache
                                                "componentstatuses")
              :to-have-same-items-as '("v1"))))
  (describe "kele--resource-namespaced-p"
    (before-each
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update kele--global-discovery-cache)))

    (it "returns t if resource is namespaced"
      (expect (kele--resource-namespaced-p kele--global-discovery-cache
                                           "v1"
                                           "pods")
              :to-be-truthy))
    (it "returns non-truthy if resource is not namespaced"
      (expect (kele--resource-namespaced-p kele--global-discovery-cache
                                           "v1"
                                           "componentstatuses")
              :not :to-be-truthy))))

(describe "kele--render-object"
  :var (fake-obj)
  (before-each
    (setq fake-obj '((kind . "FakeKind")
                     (metadata .((name . "fake-name"))))))
  (it "renders the value as YAML"
    (with-temp-buffer
      (kele--render-object fake-obj (current-buffer))
      (expect (buffer-string) :to-equal "kind: FakeKind
metadata:
  name: fake-name")))

  (describe "buffer titling"
    (describe "when input is `kele--resource-container'"
      (it "buffer name has context and namespace"
        (kele--render-object (kele--resource-container-create
                              :resource fake-obj
                              :context "fake-context"
                              :namespace "fake-namespace"))
        (expect (-map #'buffer-name (buffer-list)) :to-contain " *kele: fake-context(fake-namespace): FakeKind/fake-name*")))
    (describe "when input is regular alist"
      (it "buffer name only has kind and name"
        (kele--render-object fake-obj)
        (expect (-map #'buffer-name (buffer-list)) :to-contain " *kele: FakeKind/fake-name*")))))
;;; test-kele.el ends here
