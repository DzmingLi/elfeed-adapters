EMACS ?= emacs

.PHONY: test compile check

test:
	$(EMACS) --batch -Q \
	  -L . \
	  -L test \
	  -l elfeed-adapters-test.el \
	  -f ert-run-tests-batch-and-exit

compile:
	$(EMACS) --batch -Q \
	  -L . \
	  -f batch-byte-compile \
	  elfeed-adapters.el \
	  elfeed-adapters-http.el

check: compile test
