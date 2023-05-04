LISP ?= sbcl

all: build

# Install some Quicklisp dependencies.
ql-deps:
	# 2023-03: we want str:ensure-suffix, not yet in Quicklisp.
	git clone https://github.com/vindarel/cl-str/ ~/quicklisp/local-projectsi

# Install some sytem dependencies.
debian-deps:
	apt-get install -y libmagic-dev	libinotifytools0

macos-deps:
	echo "fsevent (for file-notify)"

run:
	$(LISP) --load ciel.asd \
	     --eval '(asdf:load-system :ciel)' \
	     --eval '(in-package:ciel-user)'

image:
	$(LISP) --load build-image.lisp

build:
	$(LISP) --non-interactive \
		--eval '(ql:quickload :deploy)' \
		--eval '(ql:quickload "cl+ssl")' \
		--load ciel.asd \
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
