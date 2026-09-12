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

	# YAML library and its dependencies.
	# not on Quicklisp as of Sept, 2026.
	# (they transitively depend on alexandria and trivial-gray-streams)
	$(call git-clone-pull,https://github.com/bohonghuang/parsonic)
	$(call git-clone-pull,https://github.com/bohonghuang/buffered-streams)
	$(call git-clone-pull,https://github.com/bohonghuang/yamson)


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
