EMACS ?= emacs
#EMACS = /usr/local/bin/emacs
#EMACS = /Applications/Emacs.app/Contents/MacOS/Emacs
EMACSFLAGS :=

CASK ?= cask
CASK := EMACS=$(EMACS) $(CASK)

SRCS := wandbox.el

PACKAGE_DIR := $(shell $(CASK) package-directory)

EMACS_BATCH = $(EMACS) -batch -no-site-file $(EMACSFLAGS)
COMPILE.el = $(CASK) exec $(EMACS_BATCH) -f batch-byte-compile

.PHONY: compile test clean

all: clean compile test

compile: $(SRCS:%.el=%.elc)

%.elc: %.el $(PACKAGE_DIR)
	$(COMPILE.el) $<

$(PACKAGE_DIR): Cask
	$(CASK) install
	touch $@

test: EMACSFLAGS += -L .
test:
	$(CASK) exec $(EMACS_BATCH) \
	-l wandbox-test.el \
	-f ert-run-tests-batch-and-exit

clean:
	$(RM) *.elc
