;;;  test-kele.el --- Test Kele functionality. -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(load-file "./tests/unit/undercover-init.el")

(require 'async)
(require 'dash)
(require 'f)

(require 'kele)

(describe "kele--prune"
  (describe "when traversal path does not exist"
    (it "no-ops"
      (expect (kele--prune '((foo . 1)
                             (bar . ((baz . 2))))
                           'foo 'baz)
              :to-equal
              '((foo . 1)
                (bar . ((baz . 2))))))))

(describe "kele--groupversion-string"
  (it "properly handles 'core API', i.e. nil group"
    (expect (kele--groupversion-string nil "v1") :to-equal "v1"))
  (it "properly forms the group-version string"
    (expect (kele--groupversion-string "apps" "v1") :to-equal "apps/v1")))

(describe "kele--groupversion-split"
  (it "properly handles core API group-version strings"
    (expect (kele--groupversion-split "v1") :to-equal '(nil "v1")))
  (it "properly splits the group-version string"
    (expect (kele--groupversion-split "apps/v1") :to-equal '("apps" "v1"))))

(describe "kele--with-progress"
  (it "returns the retval of the last evaluated sexp"
    (expect (kele--with-progress "foobar" (= 1 1)) :to-equal t)))

(describe "kele--proxy-manager"
  :var (manager)

  (describe "proxy-active-p"
    (before-each
      (setq manager
            (kele--proxy-manager
             :records (list (cons "context-a" (kele--proxy-record-create
                                               :process :fake-proc))
                            (cons "context-b" nil)))))

    (it "evals to non-nil if context present"
      (expect (proxy-active-p manager "context-a") :to-be-truthy))

    (it "evals to nil if context not present"
      (expect (proxy-active-p manager "other-context") :not :to-be-truthy)
      (expect (proxy-active-p manager "context-b") :not :to-be-truthy))))

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
    (expect (kele--context-annotate "development") :to-equal " (development-cluster, https://123.456.789.0:9999, Proxy OFF)")))

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

(describe "kele--get-host-for-context"
  (it "returns the correct value"
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache))
    (expect (kele--get-host-for-context "development") :to-equal "123.456.789.0:9999")
    (expect (kele--get-host-for-context "no-namespace") :to-equal "111.111.111.111")))

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
    (spy-on 'proxy-get :and-return-value 'fake-proc)
    (spy-on 'run-with-timer :and-return-value 'fake-timer)
    (spy-on 'cancel-timer)
    (setq kele--global-proxy-manager (kele--proxy-manager)))

  (describe "when ephemeral is nil"
    (it "adds an entry with no timer"
      (expect (kele-proxy-start "foobar" :port 9999 :ephemeral nil)
              :to-equal
              (kele--proxy-record-create
               :process 'fake-proc
               :timer nil
               :port 9999))
      (expect (assoc "foobar" (oref kele--global-proxy-manager records))
              :to-equal
              `("foobar" . ,(kele--proxy-record-create
                             :process 'fake-proc
                             :timer nil
                             :port 9999)))
      (expect 'cancel-timer :not :to-have-been-called)))

  (describe "when ephemeral is non-nil"
    (it "adds an entry with a timer"
      (kele-proxy-start "foobar" :port 9999)

      (expect (assoc "foobar" (oref kele--global-proxy-manager records))
              :to-equal
              `("foobar" .,(kele--proxy-record-create
                            :process 'fake-proc
                            :timer 'fake-timer
                            :port 9999))))))

(describe "kele--clear-namespaces-for-context"
  (before-each
    (setq kele--namespaces-cache '((foo . ("n1" "n2" "n3")))))
  (describe "when context is present in cache"
    (it "deletes the entry"
      (kele--clear-namespaces-for-context "foo")
      (expect kele--namespaces-cache :to-equal nil))))

(describe "kele--cache-namespaces"
  (before-each
    (spy-on 'run-with-timer)
    (setq kele--namespaces-cache nil)
    (setq kele-resource-refresh-overrides nil))

  (describe "when resource's cache TTL is set to :never"
    (before-each
      (setq kele-resource-refresh-overrides '((namespace . :never))))
    (it "does not create a timer"
      (kele--cache-namespaces "foobar" "n0")
      (expect 'run-with-timer :not :to-have-been-called)))

  (it "adds namespaces correctly"
    (kele--cache-namespaces "foobar" "n0" "n1" "n2")
    (expect (alist-get 'foobar kele--namespaces-cache)
            :to-equal
            '("n0" "n1" "n2"))
    (expect 'run-with-timer :to-have-been-called))
  (it "returns the namespaces list"
    (expect (kele--cache-namespaces "foobar" "n0" "n1" "n2")
            :to-equal '("n0" "n1" "n2"))))

(describe "resource caching"
  (before-each
    (setq kele-resource-default-refresh-interval 60)
    (setq kele-resource-refresh-overrides '((foo . 999)
                                            (bar . :never))))

  (describe "when a resource has a TTL override"
    (it "uses the override value"
      (expect (kele--get-cache-ttl-for-resource 'bar) :to-equal :never)
      (expect (kele--get-cache-ttl-for-resource 'foo) :to-equal 999)))
  (describe "when a resource has no TTL override"
    (it "uses the default value"
      (expect (kele--get-cache-ttl-for-resource 'qux) :to-equal 60))))

(describe "kele--cache-update (kele--discovery-cache)"
  (describe "the retval"
    (it "is keyed on host"
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update kele--global-discovery-cache))

      (expect (map-keys (oref kele--global-discovery-cache contents)) :to-have-same-items-as '("123.456.789.0_9999")))

    (it "contains the expected resources"
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update kele--global-discovery-cache))
      (let* ((api-resource-lists (alist-get "123.456.789.0_9999" (oref kele--global-discovery-cache contents) nil nil #'equal))
             (resource-lists (-map (lambda (arl) (alist-get 'resources arl))
                                   api-resource-lists))
             (resources (-flatten-n 1 resource-lists))
             (names (-map (lambda (r) (alist-get 'name r)) resources)))
        (expect (length api-resource-lists) :to-equal 3)
        (expect (--map (let-alist it .groupVersion) api-resource-lists)
                :to-have-same-items-as
                '("v1" "fake-group/v1" "fake-other-group/v1"))
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
  (before-each
    (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
    (async-wait (kele--cache-update kele--global-discovery-cache)))

  (it "optionally filters for resources that support a given verb"
    (expect (kele--get-resource-types-for-context
             "development"
             :verb 'deletecollection)
            :to-have-same-items-as
            '("configmaps"
              "nodes"
              "pods"
              "replicationcontrollers"
              "resourcequotas"
              "secrets"
              "serviceaccounts")))

  (it "contains the expected resources"
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
      (spy-on 'kele--cache-update)
      (spy-on 'run-with-timer :and-return-value :timer)
      (kele--cache-start cache :bootstrap t))

    (it "sets the timer"
      (expect (oref cache timer) :to-equal :timer)))

  (describe "kele--get-singular-for-plural"
    (before-each
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update kele--global-discovery-cache)))

    (it "fetches singularName"
      (expect
       (kele--get-singular-for-plural kele--global-discovery-cache
      "ambiguousthings")
       :to-equal
       "ambiguousthing"))

    (describe "when singularName not present"
      (it "returns lowercased .metadata.name"
        (expect (kele--get-singular-for-plural kele--global-discovery-cache
                                               "componentstatuses")
                :to-equal
                "componentstatus"))))

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
      (setq cache (kele--discovery-cache))
      (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
      (async-wait (kele--cache-update cache)))

    (it "errors if resource not found"
      (expect (kele--resource-namespaced-p cache
                                           "v1"
                                           "foobar")
              :to-throw 'kele-cache-lookup-error))

    (it "returns t if resource is namespaced"
      (expect (kele--resource-namespaced-p cache
                                           "v1"
                                           "pods")
              :to-be-truthy))
    (it "returns non-truthy if resource is not namespaced"
      (expect (kele--resource-namespaced-p cache
                                           "v1"
                                           "componentstatuses")
              :not :to-be-truthy)))

  (describe "kele--resource-has-verb-p"
    (describe "when resource has the argument verb"
      (it "returns truthy"
        (expect (kele--resource-has-verb-p
                 cache "v1" "componentstatuses" 'get)
                :to-be-truthy)))
    (describe "when resource does not have the argument verb"
      (it "returns nil"
        (expect (kele--resource-has-verb-p
                 cache "v1" "componentstatuses" 'delete)
                :not :to-be-truthy)))))

(describe "kele--render-object"
  :var (fake-obj)
  (before-each
    (setq kele-get-show-instructions nil)
    (setq fake-obj '((kind . "FakeKind")
                     (metadata .((name . "fake-name"))))))
  (it "renders the value as YAML"
    (with-temp-buffer
      (kele--render-object fake-obj (current-buffer))
      (expect (buffer-string) :to-equal "kind: FakeKind
metadata:
  name: fake-name")))

  (it "respects `kele-filtered-fields'"
    (setq fake-obj '((kind . "FakeKind")
                     (metadata . ((name . "fake-name")
                                  (foo . bar)))))
    (let ((kele-filtered-fields '((metadata foo))))
      (with-temp-buffer
        (kele--render-object fake-obj (current-buffer))
        (expect (buffer-string) :to-equal "kind: FakeKind
metadata:
  name: fake-name"))))

  (describe "buffer titling"
    (describe "when input is `kele--resource-container'"
      (it "buffer name has context and namespace"
        (kele--render-object (kele--resource-container-create
                              :resource fake-obj
                              :context "fake-context"
                              :namespace "fake-namespace"))
        (expect (-map #'buffer-name (buffer-list)) :to-contain "*kele: fake-context(fake-namespace): FakeKind/fake-name*"))
      (describe "when the resource is not namespaced"
        (it "buffer name only shows context, kind, and name"
          (kele--render-object (kele--resource-container-create
                                :resource fake-obj
                                :context "fake-context"
                                :namespace nil))
          (expect (-map #'buffer-name (buffer-list)) :to-contain "*kele: fake-context: FakeKind/fake-name*"))))
    (describe "when input is regular alist"
      (it "buffer name only has kind and name"
        (kele--render-object fake-obj)
        (expect (-map #'buffer-name (buffer-list)) :to-contain "*kele: FakeKind/fake-name*")))))

(describe "kele--get-resource"
  (before-each
    (spy-on 'plz)
    (spy-on 'proxy-start
            :and-return-value
            (kele--proxy-record-create
             :port 9999))
    (setq kele-cache-dir (f-expand "./tests/testdata/cache"))
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-discovery-cache))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache)))

  (describe "when resource is not namespaced"
    (it "errors when namespace is provided anyway"
      (expect (kele--get-resource (kele--gvk-create :kind "nodes")
                                  "my-node"
                                  :namespace "foobar")
              :to-throw 'user-error))
    (it "calls the right endpoint"
      (expect (kele--get-resource
               "nodes" "my-node"
               :context "development")
              :not :to-throw)
      (expect 'plz :to-have-been-called-with
              'get
              "http://localhost:9999/api/v1/nodes/my-node"
              :as #'json-read)))

  (describe "when GROUP and VERSION not specified"
    (describe "when only one group-version exists for the argument resource type"
      (it "auto-selects group-version"
        (kele--get-resource "resourcequotas" "my-rq" :namespace "foobar")
        (expect 'plz :to-have-been-called-with
                'get
                "http://localhost:9999/api/v1/namespaces/foobar/resourcequotas/my-rq"
                :as #'json-read)))

    (describe "when multiple group-versions exist for the same resource type"
      ;; This would allow consumers of this API to decide if they'd like to
      ;; disambiguate on users' behalf, present a completion buffer to users to
      ;; select, etc.
      (it "errors with the group-version options attached to the error"
        (expect (kele--get-resource "ambiguousthings" "fake-name")
                :to-throw 'kele-ambiguous-groupversion-error)
        (condition-case err
            (kele--get-resource "ambiguousthings" "fake-name")
          (kele-ambiguous-groupversion-error
           (expect (cdr err) :to-have-same-items-as '("fake-group/v1"
                                                      "fake-other-group/v1")))
          (:success (buttercup-fail "Received unexpected success")))
        (expect 'kele--ensure-proxy :not :to-have-been-called)))))

(defun kele-test--noop (&rest _)
  "Do nothing."
  nil)

(describe "kele--transient-scope-mutator"
  :var (infix)
  (before-each
    (setq transient--prefix (transient-prefix :incompatible nil :scope 'fake-scope))
    (setq infix (kele--transient-scope-mutator :argument "--foo=" :fn 'kele-test--noop))
    (spy-on 'kele-test--noop :and-call-through)
    (transient-infix-set infix "foo"))

  (it "calls the property function when setting a new value"
    (expect 'kele-test--noop :to-have-been-called-with 'fake-scope "foo"))
  (it "sets the value of the actual infix properly"
    (expect (oref infix value) :to-equal "foo")))

(describe "kele--transient-infix-resetter"
  :var (infix other0 other1)
  (before-each
    (spy-on 'transient-init-value)
    (setq other0 (transient-option :argument "--foo"))
    (setq other1 (transient-option :argument "--bar"))
    (setq transient--suffixes `(,other0 ,other1)))
  (it "re-initializes the value of the specified infix"
    (transient-infix-set
     (kele--transient-infix-resetter :resettees '("--foo") :argument "--whatever=")
     "value")
    (expect 'transient-init-value :to-have-been-called-with other0)))

(describe "kele--transient-switches"
  :var (switch)
  (before-each
    (spy-on 'transient--show)
    (setq switch (kele--transient-switches
                  :options (lambda () '("foo" "bar"))
                  :argument "argument="))
    (transient-init-value switch))

  (it "starts at the first item in the list"
    (expect (oref switch value) :to-equal "foo"))

  (describe "transient-infix-value"
    (it "formats as <argument>=<value>"
      (expect (transient-infix-value switch) :to-equal "argument=foo")))

  (describe "transient-infix-read"
    (it "returns the next choice in the list"
      (expect (transient-infix-read switch) :to-equal "bar"))
    (describe "when current selection is the last item"
      (before-each
        (transient-infix-set switch "bar"))
      (it "loops back to the first item in the list"
        (expect (transient-infix-read switch) :to-equal "foo")))))

(describe "kele-mode"
  (describe "enabling"
    (describe "when `kele-enabled' is truthy"
      (before-each
        (setq kele--enabled t)
        (spy-on 'kele--cache-start)
        (spy-on 'kele--setup-embark-maybe))
      (it "does nothing"
        (kele-mode 1)
        (expect 'kele--cache-start :not :to-have-been-called)
        (expect 'kele--setup-embark-maybe :not :to-have-been-called)))
    (describe "when `kele-kubectl-executable' not found"
      (before-each
        (spy-on 'executable-find
                :and-call-fake
                (lambda (exec)
                  (not (string-equal exec kele-kubectl-executable)))))
      (it "throws error"
        (expect (kele-mode 1) :to-throw 'error)))
    (describe "when `kele-kubeconfig-path' does not exist"
      (before-each
        (spy-on 'file-exists-p
                :and-call-fake
                (lambda (f)
                  (not (string-equal f kele-kubeconfig-path)))))
      (it "throws error"
        (expect (kele-mode 1) :to-throw 'error))))
  (describe "disabling"
    (describe "when `kele--enabled' is nil"
      (before-each
        (setq kele--enabled nil)
        (spy-on 'kele--cache-stop)
        (spy-on 'kele--teardown-embark-maybe))
      (it "does nothing"
        (kele-mode -1)
        (expect 'kele--cache-stop :not :to-have-been-called)
        (expect 'kele--teardown-embark-maybe :not :to-have-been-called)))))

(describe "kele--get-context-arg"
  (before-each
    (spy-on 'kele-current-context-name :and-return-value "bar"))
  (describe "when called in a Transient suffix"
    (before-each
      (setq transient-current-command t)
      (spy-on 'transient-args :and-return-value '("--context=foo")))
    (it "retrieves from the suffix's arguments"
      (expect (kele--get-context-arg) :to-equal "foo")))
  (describe "when called outside of a Transient"
    (before-each
      (setq transient-current-command nil))
    (it "retrieves the current context"
      (expect (kele--get-context-arg) :to-equal "bar"))))

(describe "kele--get-namespace-arg"
  (before-all
    (setq kele-kubeconfig-path (f-expand "./tests/testdata/kubeconfig.yaml"))
    (async-wait (kele--cache-update kele--global-kubeconfig-cache)))
  (describe "when called in a Transient suffix"
    (before-each
      (setq transient-current-command t)
      (spy-on 'transient-args :and-return-value '("--namespace=foo")))
    (it "retrieves from the suffix's arguments"
      (expect (kele--get-namespace-arg) :to-equal "foo")))
  (describe "when called outside of a Transient"
    (describe "when USE-DEFAULT is non-nil"
      (before-each
        (setq transient-current-command nil))
      (it "retrieves the default namespace of the current context"
        (expect (kele--get-namespace-arg :use-default t) :to-equal "development-namespace")))

    (describe "when USE-DEFAULT is nil"
      (describe "when GROUP-VERSION and KIND is not namespaced"
        (before-each
          (spy-on 'kele--resource-namespaced-p :and-return-value nil))
        (it "returns nil"
          (expect (kele--get-namespace-arg :group-version "apps/v1" :kind "foo")
                  :not :to-be-truthy)))

      (describe "when GROUP-VERSION and KIND are namespaced"
        (before-each
          (spy-on 'kele--resource-namespaced-p :and-return-value t)
          (spy-on 'completing-read))
        (it "prompts user for namespace selection"
          (spy-on 'kele--get-namespaces)
          (kele--get-namespace-arg :group-version "apps/v1" :kind "foo")
          (expect 'completing-read :to-have-been-called))))))

(describe "kele--get-kind-arg"
  (describe "when called in a Transient suffix"
    (it "returns the kind set on the scope"
      (setq transient-current-prefix
            (transient-prefix :scope '((kind . "foo"))))
      (expect (kele--get-kind-arg) :to-equal "foo"))))

(describe "kele--get-groupversion-arg"
  (describe "when called in a Transient buffer"
    (before-each
      (setq transient-current-command t))
    (describe "when the current command has a `--groupversion' arg"
      (before-each
        (spy-on 'transient-args :and-return-value '("--groupversion=foo")))

      (it "returns that value"
        (expect (kele--get-groupversion-arg) :to-equal "foo"))))

  (describe "when the kind is of unambiguous group-version"
    (before-each
      (spy-on 'kele--get-groupversions-for-type :and-return-value '("foo/v1")))
    (it "returns the group-version"
      (expect (kele--get-groupversion-arg) :to-equal "foo/v1")))

  (describe "when the kind has multiple group-versions"
    (before-each
      (spy-on 'kele--get-groupversions-for-type :and-return-value '("foo/v1" "bar/v1")))
    (it "prompts user for completion"
      (spy-on 'completing-read)
      (kele--get-groupversion-arg)
      (expect 'completing-read :to-have-been-called))))

(describe "kele--gvk-string"
  (it "formats w/o group"
    (expect (kele--gvk-string nil "v1" "Pod") :to-equal "v1.Pod"))
  (it "formats w/ group"
    (expect (kele--gvk-string "group" "v1" "Pod") :to-equal "group/v1.Pod")))

 ;;; test-kele.el ends here
