
# Install

Once you have it installed, see the next section on how to create your package definition with `CIEL`.

## With Quicklisp

You need a Lisp implementation and Quicklisp installed.

CIEL is not yet on Quicklisp (but it is on [Ultralisp](https://ultralisp.org)), so clone this repository and load the .asd (with `load` or `C-c C-k` in Slime).

``` example
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


## With a binary. Use CIEL's custom REPL.

You don't need anything, just download the CIEL executable and run its REPL.

TODO: build it on CI for different platforms.

To build it, clone this repository and run `make build`.

Start it with `./ciel`.

You are dropped into a custom Lisp REPL, freely based on [sbcli](https://github.com/hellerve/sbcli).

This REPL is more user friendly than the default SBCL one:

-   it has readline capabilities, meaning that the arrow keys work by default (wouhou!) and there is a persistent history, like in any shell.
-   it has **multiline input**.
-   it has **TAB completion**.
-   it handles errors gracefully: you are not dropped into the debugger and its sub-REPL, you simply see the error message.
-   it has optional **syntax highlighting**.

    It also defines short helper commands:

``` txt

%help => Prints this general help message
%doc => Prints the available documentation for this symbol
%? => Gets help on a symbol <sym>: :? str
%w => Writes the current session to a file <filename>
%d => Dumps the disassembly of a symbol <sym>
%t => Prints the type of a expression <expr>
%q => Ends the session.
```

Note: the documentation is also available by appending a "?" after a function name:

``` txt
ciel-user> (dict ?
```

Syntax highlighting is currently off by default. To enable it, install [pygments](https://pygments.org/) and add this in your `~/.cielrc`:

``` commonlisp
(in-package :sbcli)
(setf *syntax-highlighting* t)

;; and, optionally:
;; (setf *pygmentize* "/path/to/pygmentize")
;; (setf *pygmentize-options* (list "-s" "-l" "lisp"))
```

You can also switch it on and off from the REPL:

``` commonlisp
(setf sbcli:*syntax-highlighting* t)
```

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
