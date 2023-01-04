export EMACS ?= $(shell which emacs)
CASK_DIR := $(shell cask package-directory)

$(CASK_DIR): Cask
	cask install
	@touch $(CASK_DIR)

.PHONY: cask
cask: $(CASK_DIR)

.PHONY: compile
compile: cask
	cask emacs -batch -L . -L test \
          --eval "(setq byte-compile-error-on-warn t)" \
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: checkdoc
checkdoc:
	cask emacs -batch --eval "(checkdoc-file \"kele.el\")"

.PHONY: package-lint
package-lint:
	cask emacs -batch -f package-lint-batch-and-exit kele.el

.PHONY: test
test: compile
	cask clean-elc
	cask exec buttercup -L . tests/

.PHONY: all
testall: checkdoc package-lint test

.PHONY: fake-cluster
fake-cluster:
	kind create cluster --kubeconfig ./tests/kubeconfig.yaml --config ./tests/test-cluster.yaml
	kind delete cluster --kubeconfig ./tests/kubeconfig.yaml --name kele-test-cluster

