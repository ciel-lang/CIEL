# CIEL's custom REPL

CIEL's REPL is more user friendly than the default SBCL one. In particular:

-  it has readline capabilities, meaning that the arrow keys work by default (wouhou!) and there is a persistent history, like in any shell.
-  it has **multiline input**.
-  it has **TAB completion**.
-  it handles errors gracefully: you are not dropped into the debugger and its sub-REPL, you simply see the error message.
-  it has optional **syntax highlighting**.

-  it defines short **helper commands**:

``` txt
:help => Prints this general help message
:doc => Prints the available documentation for this symbol
:? => Gets help on a symbol <sym>: :? str
:w => Writes the current session to a file <filename>
:d => Dumps the disassembly of a symbol <sym>
:t => Prints the type of a expression <expr>
:lisp-critic => Toggles the lisp-critic
:q => Ends the session.
```

- it has a **shell pass-through**: try `!ls` (available in the `ciel-user` package)

Our REPL is adapted from [sbcli](https://github.com/hellerve/sbcli). See also [cl-repl](https://github.com/koji-kojiro/cl-repl/), that has an interactive debugger.

> Note: a shell interface doesn't replace a good development environment. See this [list of editors for Common Lisp](https://lispcookbook.github.io/cl-cookbook/editor-support.html): Emacs, Vim, Atom, VSCode, SublimeText, Jupyter Notebooks and more.

## Quick documentation lookup

The documentation for a symbol is available with `:doc` and also by
appending a "?" after a function name:

```
ciel-user> :doc dict
;; or:
ciel-user> (dict ?
```

## Shell pass-through

Use `!` to send a shell command:

```
!ls
Makefile
README.org
repl.lisp
repl-utils.lisp
src
...

!pwd
/home/vindarel/projets/ciel
```

Use square brackets `[...]` to write a shell script, and use `$` inside it to escape to lisp:

```lisp
(dotimes (i 7) (princ [echo ?i]))
```

The result is concatenated into a string and printed on stdout.

This feature is only available in CIEL's REPL, not on the CIEL-USER package.

We use the [Clesh](https://github.com/Neronus/clesh) library.

See also [SHCL](https://github.com/bradleyjensen/shcl) for a more unholy union of posix-shell and Common Lisp.


## Syntax highlighting

Syntax highlighting is off by default. To enable it, install [pygments](https://pygments.org/) and add this in your `~/.cielrc`:

```lisp
(in-package :sbcli)
(setf *syntax-highlighting* t)

;; and, optionally:
;; (setf *pygmentize* "/path/to/pygmentize")
;; (setf *pygmentize-options* (list "-s" "-l" "lisp"))
```

You can also switch it on and off from the REPL:

```lisp
(setf sbcli:*syntax-highlighting* t)
```

## Friendly lisp-critic

The `:lisp-critic` helper command toggles on and off the
[lisp-critic](https://github.com/g000001/lisp-critic). The Lisp Critic
scans your code for instances of bad Lisp programming practice. For
example, when it sees the following function:


~~~lisp
(critique
   (defun count-a (lst)
     (setq n 0)
     (dolist (x lst)
       (if (equal x 'a)
         (setq n (+ n 1))))
     n))
~~~

the lisp-critic gives you these advices:

```
----------------------------------------------------------------------

SETS-GLOBALS: GLOBALS!! Don't use global variables, i.e., N
----------------------------------------------------------------------

DOLIST-SETF: Don't use SETQ inside DOLIST to accumulate values for N.
Use DO. Make N a DO variable and don't use SETQ etc at all.
----------------------------------------------------------------------

USE-EQL: Unless something special is going on, use EQL, not EQUAL.
----------------------------------------------------------------------

X-PLUS-1: Don't use (+ N 1), use (1+ N) for its value or (INCF N) to
change N, whichever is appropriate here.
----------------------------------------------------------------------
; in: DEFUN COUNT-A
;     (SETQ CIEL-USER::N 0)
;
; caught WARNING:
;   undefined variable: N
;
; compilation unit finished
;   Undefined variable:
;     N
;   caught 1 WARNING condition
=> COUNT-A
```
