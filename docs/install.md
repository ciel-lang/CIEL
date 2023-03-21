
# Install

Once you have it installed, see the next section on how to create your package definition with `CIEL`.

## With a binary.

You don't need anything, just download the CIEL executable and run its REPL.

- we provide an experimental binary for Debian systems: go to
https://gitlab.com/vindarel/ciel/-/pipelines, download the latest
artifact, unzip the `ciel-v0.zip` archive and run `ciel-v0/ciel`.

To build it, clone this repository and run `make build`.

Start it with `./ciel`.

You are dropped into a custom Lisp REPL.

To run a .lisp file as a script, give it as argument:

    ciel myscript.lisp

See the next sections for usage documentation.


## With Quicklisp

If you go this route, you need a Lisp implementation and Quicklisp installed.

CIEL is not yet on Quicklisp (but it is on [Ultralisp](https://ultralisp.org)), so clone this repository and load the .asd (with `load` or `C-c C-k` in Slime).

``` example
# Get a dependency that is not up to date on Quicklisp:
git clone https://github.com/vindarel/cl-str ~/quicklisp/local-projects/  # or use Ultralisp
# Get CIEL proper:
git clone https://github.com/ciel-lang/CIEL ~/quicklisp/local-projects/CIEL
```

Then, quickload it:

``` commonlisp
(ql:quickload "ciel")
```

and enter the `ciel-user` package, instead of the default `common-lisp-user` (or `cl-user`):

``` commonlisp
(in-package :ciel-user)
```

## With a core image

You need a Lisp implementation, but you don't need Quicklisp.

Build a *core image* for your lisp with all CIEL's dependencies:

``` example
sbcl --load build-image.lisp
```

and use it:

``` example
sbcl --core ciel-core --eval '(in-package :ciel-user)'
```

Note: you must build the core image, we can't distribute ready-to-use core images, it must be built on your machine.


# Use in the REPL and in new packages

On the REPL, enter the `ciel-user` package instead of `cl-user` (`,in-package RET ciel-user`).

Use CIEL in your own packages by `use`-ing it in addition of `cl`:

~~~lisp
(defpackage yourpackage
  (:use :cl :ciel))
~~~

You can also use `generic-ciel`, based on [generic-cl](https://github.com/alex-gutev/generic-cl/):

~~~lisp
(defpackage yourpackage
  (:use :cl :generic-ciel))
~~~

`generic-cl` allows us to define our `+` or `equalp` methods for our
own objects (and more).
