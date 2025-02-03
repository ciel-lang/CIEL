LISP ?= sbcl

all: build

# can use bespoke dir like 'QLDIR=~/nostandard/local-projects make'
QLDIR ?= $(HOME)/quicklisp/local-projects

# make will exit early if git clone errors b/c dir already exists
define git-clone-pull =
if test -d $(QLDIR)/$(notdir $1); then cd $(QLDIR)/$(notdir $1) && git pull; else git clone $1 $(QLDIR)/$(notdir $1); fi
endef

$(QLDIR)/asdf:
	# 2024-08: building with older asdf fails
	# unrecognized define-package keyword :LOCAL-NICKNAMES
	# https://github.com/ciel-lang/CIEL/issues/58
	mkdir -p $(QLDIR)
	cd $(QLDIR) && \
		curl -sL https://asdf.common-lisp.dev/archives/asdf-3.3.5.tar.gz | \
		tar -xvzf - && \
		mv asdf-3.3.5 asdf

asdf: $(QLDIR)/asdf
	@echo "New ASDF version installed to " $(QLDIR)

check-asdf-version:
	sbcl --script check-asdf-version.lisp || echo "Your ASDF version is too old. You can update it with 'make asdf'. It will be downloaded to " $(QLDIR) ". You can set QLDIR."

# Install some Quicklisp dependencies.
ql-deps: check-asdf-version
	# termp, little utility, it is NOT in Quicklisp as of <2025-02-02>.
	# I asked for inclusion in Quicklisp.
	$(call git-clone-pull,https://github.com/vindarel/termp)

	# 2023-11: The symbol SB-INT:TRULY-DYNAMIC-EXTENT is absent since at least
	# SBCL v2.3.10, which was required in older versions of cl-environments
	# and cl-form-types.
	# See issue https://github.com/ciel-lang/CIEL/issues/38
	# This has been fixed upstream, it is in Quicklisp 2024-08
	$(call git-clone-pull,https://github.com/alex-gutev/cl-environments)
	$(call git-clone-pull,https://github.com/alex-gutev/cl-form-types)

	# 2024-08: Moira needs moira/light, added <2023-11-23 Thu>,
	# it is in Quicklisp 2024-08
	# moira/light doesn't depend on Osicat.
	$(call git-clone-pull,https://github.com/ruricolist/moira)

	# 2024-08: simple progress bar, it is in Quicklisp 2024-08.
	$(call git-clone-pull,https://github.com/vindarel/progressons)

	# In Quicklisp 2024-08.
	$(call git-clone-pull,https://github.com/lisp-maintainers/file-finder)

	# <2024-08-30> error with SBCL: Lock on package SB-DI violated…
	# fixed https://github.com/Shinmera/dissect/issues/18 on March, 2024, in Quicklisp 2024-08.
	$(call git-clone-pull,https://github.com/Shinmera/dissect)

	# fix fset on latest SBCL
	# "Lock on package SB-EXT violated when interning ONCE-ONLY while in package FSET"
	# see https://github.com/slburson/fset/pull/46
	$(call git-clone-pull,https://gitlab.common-lisp.net/misc-extensions/misc-extensions)
	$(call git-clone-pull,https://github.com/slburson/fset)

# Install some system dependencies.
debian-deps:
	apt-get install -y libinotifytools0

macos-deps:
	echo "please install fsevent (for file-notify)"

run:
	$(LISP) --load ciel.asd \
	     --eval '(asdf:load-system :ciel)' \
	     --eval '(in-package :ciel-user)'

run-repl:
	$(LISP) --load ciel.asd \
		--eval '(asdf:load-system :ciel)' \
		--eval '(asdf:load-system :ciel/repl)' \
		--eval '(sbcli:repl)'

image:
	$(LISP) --load build-image.lisp

build:
	$(LISP) --non-interactive \
		--eval '(ql:quickload "cl+ssl")' \
		--load ciel.asd \
		--eval '(ql:quickload :swank)' \
	     --eval '(ql:quickload :ciel)' \
	     --eval '(ql:quickload :ciel/repl)' \
	     --eval '(asdf:make :ciel/repl)' \
	     --eval '(quit)'

gen-dependencies-list:
	./find-dependencies.lisp > docs/dependencies.md

serve-docs:
	docsify serve docs/

clean:
	rm ciel
