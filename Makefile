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
	  -f batch-byte-compile $$(cask files); \
	  (ret=$$? ; cask clean-elc && exit $$ret)

.PHONY: package-lint
package-lint:
	cask emacs -batch -f package-lint-batch-and-exit kele.el

.PHONY: test
test: compile
	cask clean-elc
	cask exec buttercup -L . tests/
