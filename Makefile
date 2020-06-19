.PHONY: install
install:
	cask install

.PHONY: test
test:
	cask exec ert-runner

.PHONY: docs
docs:
	cask exec emacs -batch -l create-docs.el -f create-docs
