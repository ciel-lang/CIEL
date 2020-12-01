LISP ?= sbcl

run:
	$(LISP) --load ciel.asd \
	     --eval '(asdf:load-system :ciel)' \
	     --eval '(in-package:ciel-user)'

image:
	$(LISP) --load build-image.lisp

build:
	$(LISP) --load ciel.asd \
		--eval '(ql:quickload :swank)' \
	     --eval '(ql:quickload :ciel)' \
	     --eval '(asdf:make :ciel)' \
	     --eval '(quit)'

serve-docs:
	docsify serve docs/
