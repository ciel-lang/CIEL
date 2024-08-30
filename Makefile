LISP ?= sbcl

all: build

# Install some Quicklisp dependencies.
ql-deps:
	# 2023-11: The symbol SB-INT:TRULY-DYNAMIC-EXTENT is absent since at least
	# SBCL v2.3.10, which was required in older versions of cl-environments
	# and cl-form-types.
	# See issue https://github.com/ciel-lang/CIEL/issues/38
	# This has been fixed upstream, not yet in Quicklisp
	git clone https://github.com/alex-gutev/cl-environments ~/quicklisp/local-projects/cl-environments
	git clone https://github.com/alex-gutev/cl-form-types ~/quicklisp/local-projects/cl-form-types

	# 2024-08: Moira needs moira/light, added <2023-11-23 Thu>, not on Quicklisp…
	# moira/light doesn't depend on Osicat.
	git clone https://github.com/ruricolist/moira/ ~/quicklisp/local-projects/moira

	# 2024-08: simple progress bar, not in Quicklisp.
	git clone https://github.com/vindarel/progressons ~/quicklisp/local-projects/progressons

	# 2024-08: not in Quicklisp
	git clone https://github.com/lisp-maintainers/file-finder ~/quicklisp/local-projects/file-finder

# Install some system dependencies.
debian-deps:
	apt-get install -y libinotifytools0

macos-deps:
	echo "please install fsevent (for file-notify)"

run:
	$(LISP) --load ciel.asd \
	     --eval '(asdf:load-system :ciel)' \
	     --eval '(in-package :ciel-user)'

image:
	$(LISP) --load build-image.lisp

build:
	$(LISP) --non-interactive \
		--eval '(ql:quickload :deploy)' \
		--eval '(ql:quickload "cl+ssl")' \
		--load ciel.asd \
		--load build-config.lisp \
		--eval '(ql:quickload :swank)' \
	     --eval '(push :deploy-console *features*)' \
	     --eval '(ql:quickload :ciel)' \
	     --eval '(ql:quickload :ciel/repl)' \
	     --eval '(asdf:make :ciel/repl)' \
	     --eval '(quit)'

gen-dependencies-list:
	./find-dependencies.lisp > docs/dependencies.md

serve-docs:
	docsify serve docs/
