EMACS ?= emacs
EMACS = /usr/local/bin/emacs
#EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs

LOAD_PATH := -L .
EMACS_BATCH := $(EMACS) -batch -no-site-file $(LOAD_PATH)

all: clean wandbox.elc test

wandbox.elc: wandbox.el
	$(EMACS_BATCH) \
	--eval "(setq byte-compile-error-on-warn t)" \
	-f batch-byte-compile $^

.PHONY: test
test: wandbox.elc
	$(EMACS_BATCH) \
	-l test-wandbox.el \
	-f ert-run-tests-batch-and-exit

.PHONY: clean
clean:
	$(RM) *.elc
