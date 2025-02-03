# CIEL's custom REPL

CIEL's REPL is more user friendly than the default SBCL one. In particular:

-  it has readline capabilities, meaning that the arrow keys work by default (wouhou!) and there is a persistent history, like in any shell.
-  it has **multiline input**.
-  it has **TAB completion**.
  - including for files (after a bracket) and binaries in the PATH.
-  it handles errors gracefully: you are not dropped into the debugger and its sub-REPL, you simply see the error message.
-  it has optional **syntax highlighting**.
- it has a **shell pass-through**: try `!ls` (also available in Slime)
  - it runs **interactive commands**: try `!htop`, `!vim test.lisp`, `!emacs -nw test.lisp` or `!env FOO=BAR sudo -i powertop`.
- it has a quick **edit and load file** command: calling `%edit file.lisp` will open the file with the editor of the EDITOR environment variable. When you close it, the file is loaded and evaluated.
- it has an optional **lisp critic** that scans the code you enter at
  the REPL for instances of bad practices.

-  it defines more **helper commands**:

``` txt
%help => Prints this general help message
%doc => Prints the available documentation for this symbol
%? => Gets help on a symbol <sym>: :? str
%w => Writes the current session to a file <filename>
%d => Dumps the disassembly of a symbol <sym>
%t => Prints the type of an expression <expr>
%q => Ends the session.
```

Our REPL is adapted from [sbcli](https://github.com/hellerve/sbcli). See also [cl-repl](https://github.com/koji-kojiro/cl-repl/), that has an interactive debugger.

> Note: a shell interface doesn't replace a good development environment. See this [list of editors for Common Lisp](https://lispcookbook.github.io/cl-cookbook/editor-support.html): Emacs, Vim, Atom, VSCode, Intellij, SublimeText, Jupyter Notebooks and more.

## Quick documentation lookup

The documentation for a symbol is available with `%doc` and also by
appending a "?" after a function name:

```
ciel-user> %doc dict
;; or:
ciel-user> (dict ?
```

## Shell pass-through with "!"

Use `!` to send a shell command. All shell commands are run interactively, so you can run `htop`, `sudo`, `emacs -nw` etc.

```
!ls
!sudo emacs -nw /etc/
```

We provide TAB completion for shell commands that are in your PATH.

See [Lish](https://github.com/nibbula/lish/) and [SHCL](https://github.com/bradleyjensen/shcl) for more unholy union of (posix) shells and Common Lisp.


## Syntax highlighting

Syntax highlighting is off by default. To enable it, install [pygments](https://pygments.org/) and add this in your `~/.cielrc`:

```lisp
(setf sbcli:*syntax-highlighting* t)

;; and, optionally:
;; (setf sbcli::*pygmentize* "/path/to/pygmentize")
;; (setf sbcli::*pygmentize-options* (list "-s" "-l" "lisp"))
```

You can also switch it on and off from the REPL:

```lisp
(setf sbcli:*syntax-highlighting* t)
```

## Friendly lisp-critic

The `%lisp-critic` helper command toggles on and off the
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

## Quick edit & load a file

Use `%edit file.lisp`.

This will open the file with the editor of the EDITOR environment variable. When you
close it, the file is loaded and evaluated. If you defined functions, you can try them in the REPL.

It is a quick way to write lisp code and have fast feedback. It's nice
to use to tinker with code, to write small throw-away
programs. However, this doesn't replace a true editor setup!

We use [magic-ed](https://github.com/sanel/magic-ed). You can also call it manually with `(magic-ed "file.lisp")`, and give it a couple arguments:

- `:eval nil`: don't evaluate the file when you close it.
- `:output :string`: output the file content as a string.
