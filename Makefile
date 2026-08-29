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
	  elfeed-adapters-http.el \
	  elfeed-adapters-douban.el \
	  elfeed-adapters-gcores.el \
	  elfeed-adapters-netease-music.el \
	  elfeed-adapters-telegram.el \
	  elfeed-adapters-theatlantic.el \
	  elfeed-adapters-zhihu.el

check: compile test
