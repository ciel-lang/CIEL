# CIEL's custom REPL

CIEL's REPL is more user friendly than the default SBCL one. In particular:

-  it has readline capabilities, meaning that the arrow keys work by default (wouhou!) and there is a persistent history, like in any shell.
-  it has **multiline input**.
-  it has **TAB completion**.
  - including for files (after a bracket) and binaries in the PATH.
-  it handles errors gracefully: you are not dropped into the debugger and its sub-REPL, you simply see the error message.
-  it has optional **syntax highlighting**.
- it has a **shell pass-through**: try `!ls` (also available in Slime)
  - you must activate it with `(enable-shell-passthrough)`
  - you can mix and match shell and Lisp: try `!echo ?(+ 1/3 1/3)` (look, a fraction)
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

Activate it with `(enable-shell-passthrough)`.

Use `!` to send a shell command:

```
!ls
!sudo emacs -nw /etc/
```

### Mixing Lisp code with "?"

You can mix shell commands and Lisp code. The `?` character is the
"lisp escape". For example:

    * !echo ?(+ 2 3)

or:

    * (defun factorial (x) (if (zerop x) 1 (* x (factorial (1- x)))))
    * !echo "fact 3: " ?(factorial 3)

Escape the "?" with "\?" to write it in a shell command.

### Shell commands in Slime and limitations

The "!" shell pass-through is not enabled by default. You can enable both in the terminal REPL and in Slime:

      CIEL-USER> (enable-shell-passthrough)

There are differences on how shell commands are handled in the terminal REPL and in Slime.

All shell commands in the terminal are run interactively. You can see
the program output as it goes. In Emacs and Slime, the commands are
run *synchronously*, the output (and error output) is captured and
displayed when the command is finished.

In the terminal REPL, you can use `sudo`, `emacs -nw` and other visual
and interactive commands, but not in Slime.

> Note: the shell-passthrough feature is experimental.

> Note: we encourage our users to use a good editor rather than a terminal!

We use our fork of the [Clesh](https://github.com/lisp-maintainers/clesh) library.

See also [Lish](https://github.com/nibbula/lish/) and [SHCL](https://github.com/bradleyjensen/shcl) for more unholy union of (posix) shells and Common Lisp.


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
