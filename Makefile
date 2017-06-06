emacs ?= emacs

all: test

test: clean
	cask exec emacs -Q -batch -l test/unit-test.el -l test/indentation-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f groovy-mode.elc

.PHONY:	all test
