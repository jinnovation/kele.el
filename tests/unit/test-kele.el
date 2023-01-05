;;;  test-kele.el --- Test Kele functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/unit/undercover-init.el")

(require 'async)
(require 'f)

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

(describe "kele-current-context-name"
  (it "returns the correct current-context value"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--update-kubeconfig))
    (expect (kele-current-context-name) :to-equal "development")))

(describe "kele--context-annotate"
  (it "returns the proper annotation text"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--update-kubeconfig))
    (expect (kele--context-annotate "development") :to-equal " (development-cluster, https://development.org/server)")))

(describe "kele-context-names"
  (it "returns the correct cluster names"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--update-kubeconfig))

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
    (async-wait (kele--update-kubeconfig)))
  (it "returns the default namespace for the current cluster"
    (spy-on 'kele-current-context-name :and-return-value "development")
    (expect (kele-current-namespace) :to-equal "development-namespace"))
  (it "returns nil if no default namespace"
    (spy-on 'kele-current-context-name :and-return-value "no-namespace")
    (expect (kele-current-namespace) :to-equal nil)))

(describe "kele--context-cluster"
  (it "returns the correct cluster"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--update-kubeconfig))
    (expect (kele--context-cluster "development") :to-equal "development-cluster")))

(describe "kele--retry"
  :var (foo)
  (before-each
    (setf (symbol-function 'foo) (lambda () nil))
    (spy-on 'foo :and-call-through))

  (it "retries the correct number of times"
    (kele--retry 'foo :count 5 :wait 0)
    (expect 'foo :to-have-been-called-times 5)))

(describe "kele--start-proxy"
  (before-each
    (spy-on 'kele--proxy-process :and-return-value 'fake-proc)
    (spy-on 'run-with-timer :and-return-value 'fake-timer)
    (setq kele--context-proxy-ledger nil))

  (describe "when ephemeral is nil"
    (it "adds an entry with no timer"
      (expect (kele--start-proxy "foobar" :port 9999 :ephemeral nil)
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
      (kele--start-proxy "foobar" :port 9999)
      (expect (alist-get 'foobar kele--context-proxy-ledger)
              :to-equal
              '((proc . fake-proc)
                (timer . fake-timer)
                (port . 9999))))))

(describe "kele--ensure-proxy"
  (before-each
    (spy-on 'kele--start-proxy)
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
      (expect 'kele--start-proxy :to-have-been-called-with "foobar"))))

(describe "kele--clear-namespaces-for-context"
  (before-each
    (setq kele--context-namespaces '((foo . ("n1" "n2" "n3")))))
  (describe "when context is present in cache"
    (it "deletes the entry"
      (kele--clear-namespaces-for-context "foo")
      (message "%s" kele--context-namespaces)
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
;;; test-kele.el ends here
