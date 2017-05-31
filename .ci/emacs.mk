# Build Emacs recipe
# Usage: make -f emacs.mk [NAME=emacs-25.1] [PREFIX=/usr/local]

PREFIX:=${HOME}/opt

EMACS:=emacs-24.4
EMACS_ARCHIVE=${EMACS}.tar.xz
EMACS_ARCHIVE_URL=http://ftpmirror.gnu.org/emacs/${EMACS_ARCHIVE}

default: build install

${EMACS_ARCHIVE}:
	curl -L -O ${EMACS_ARCHIVE_URL}

build: ${EMACS_ARCHIVE}
	tar xf ${EMACS_ARCHIVE}
	cd ${EMACS} && ./configure --without-all --without-x --prefix=${PREFIX}
	cd ${EMACS} && make -j9

install:
	cd ${EMACS} && make install-strip

.PHONY: build install
