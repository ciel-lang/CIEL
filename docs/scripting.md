# Scripting

CIEL provides a fast-starting scripting solution for Common Lisp.

It is based on a standalone binary (created with the fast SBCL
implementation) and it ships useful built-in utilities, for real-world
needs: HTTP, JSON, CSV handling, plotting, and more. You just have to
get the binary and run your script. Use a shebang line if you wish.

It's a fast and easy solution to write Lisp code for your day-to-day tasks.

> Note: this is brand new!  Expect limitations and changes.

Get the `ciel` binary (it's under 30MB) and call it with your .lisp script:

```
$ ciel script.lisp
```

(or just `./script.lisp` with a shebang line, see below)

Call built-in scripts:

```
$ ciel -s simpleHTTPserver 9000
```

> Note: script names are case insensitive.

### Example script

```lisp
#!/usr/bin/env ciel
;; optional shebang line, only for the short ./script call)

;; Start your script with this to access all CIEL goodies.
;; It is now also optional.
(in-package :ciel-user)

(defun hello (name)
  "Say hello."
  ;; format! prints on standard output and flushes the streams.
  (format! t "Hello ~a!~&" name))

;; Access CLI args:
(hello (second *script-args*))

;; We have access to the DICT notation for hash-tables:
(print "testing dict:")
(print (dict :a 1 :b 2))

;; We can run shell commands:
(cmd:cmd "ls")

;; Access environment variables:
(hello (os:getenv "USER"))  ;; os is a nickname for uiop/os

(format! t "Let's define an alias to run shell commands with '!'. This gives: ")
(defalias ! #'cmd:cmd)
(! "pwd")

;; In cas of an error, we can ask for a CIEL toplevel REPL:
(handler-case
    (error "oh no")
  (error (c)
    (format! t "An error occured: ~a" c)
    (format! t "Here's a CIEL top level REPL: ")
    (sbcli::repl :noinform t)))
```

Output:


```
$ ciel script.lisp you
=>

Hello you!
"testing dict:"

 (dict
  :A 1
  :B 2
 )
cmd? ABOUT.org	    ciel		     ciel-core
   bin  		    docs		     src
 […]
Hello vindarel!
Let's define an alias to run shell commands with '!'. This gives:
/home/vindarel/projets/ciel
ciel-user>
```

### Run (interactive) shell commands

Use [cmd](https://github.com/ruricolist/cmd):

~~~lisp
(cmd:cmd "shell command")
~~~

For interactive commands, do:

~~~lisp
(cmd:cmd "sudo htop" :input :interactive :output :interactive)
;; aka (cmd:cmd "..." :<> :interactive)
;; aka (uiop:run-program '("sudo" "htop") :output :interactive :input :interactive)
~~~

this works for `sudo`, `htop`, `less`, `vim`, `ncdu`, `fzf`, `gum` etc.


## Command line arguments

Access them with `ciel-user:*script-args*`. It is a list of strings that
contains your script name as first argument.

This list of arguments is modified by us, so that it only contains
arguments for your script, and so that it is the same list wether you
call the script with `-s` or with the shebang line. You can always
check the full original list with `(uiop:command-line-arguments)`.

You can use CL built-ins to look what's into this list, such as `(member "-h" *script-args* :test #'equal)`.

You can also use a proper command-line options parser, which is shipped with CIEL: [Clingon](https://github.com/dnaeon/clingon). This top-notch library supports:

- Short and long option names
- Automatic generation of help/usage information for commands and sub-commands
- Support for various kinds of options like *string*, *integer*, *boolean*, *switches*, *enums*, *list*, *counter*, *filepath*, etc.
- Out of the box support for `--version` and `--help` flags
- Subcommands
- Support for pre-hook and post-hook actions for commands, which allows invoking functions before and after the respective handler of the command is executed
- Support for Bash and Zsh shell completions
- and more.

See below for an example on how to use Clingon. For more, see its README and [the Cookbook: scripting page](https://lispcookbook.github.io/cl-cookbook/scripting.html#parsing-command-line-arguments).

### Parse command-line arguments with Clingon

Here's a quick example.

First, we define our options:

~~~lisp
(defparameter *cli/options*
  (list
   (clingon:make-option
    :flag                     ;; <-- option kind: a flag. Doesn't expect a parameter.
    :description "show help"
    :short-name #\h
    ;; :long-name "help"      ;; <-- already handled by Clingon for CIEL.
    :key :help))
  "Our script's options.")
~~~

Our option kinds include: `:counter`, `:string`, `:integer`, `:boolean`… and more.

Then, we define a top-level command:

~~~lisp
(defparameter *cli/command*
  (clingon:make-command
   :name "command-example"
   :description "only has a -h option, and it accepts free arguments."
   :version "0.1.0"
   :authors '("John Doe <john.doe@example.org")
   :license "AGPLv3"
   :options *cli/options* ;; <-- our options
   :handler 'cli/handler) ;; <-- our handler function
  "Our main command definition.")
~~~

Finally, we write a handler function, that reads the arguments and does something with them:

~~~lisp
(defun cli/handler (cmd)
  "Look at our CLI args and eventually start the web server."
  (let* ((help (clingon:getopt cmd :help))  ;; <-- getopt, using the :key
         (freeargs (rest (clingon:command-arguments cmd))))  ;; discard the script name.
    (when help
      (clingon:print-usage *cli/command* t)
      (return-from cli/handler))
    (when freeargs
      (log:info "you provided free arguments: " freeargs))
    ;; Run some main function:
    (main)))
~~~

Now, run everything:

~~~lisp
#+ciel
(clingon:run *cli/command* *script-args*)
~~~

An example usage:

```
$ ./myscript -h
NAME:
  example command - only has a -h option, and it accepts free arguments.

USAGE:
  command-example [options] [arguments ...]

OPTIONS:
      --help          display usage information and exit
      --version       display version and exit
  -h                  show help

AUTHORS:
  John Doe <john.doe@example.org

LICENSE:
  AGPL
```


### Options in both CIEL and your script

All free arguments *unknown to the CIEL command* coming after your script name are passed along to the script in the `*script-args*` variable. This works:

    $ ./simpleHTTPserver.lisp -b 4242

However, in the following case the "-v" option would be intercepted by the ciel binary, because it is one of its known options:

    $ ./simpleHTTPserver.lisp -v -b 4242

To give "-v" to your script, use a double slash, as in:

    $ ./simpleHTTPserver.lisp -- -v -b 4242

Pull requests are accepted to make this better.


## Executable file and shebang line

We can also make a CIEL file executable and run it directly, like this:

```
$ chmod +x script.lisp
$ ./script.lisp
```

Add the following shebang at the beginning:

```sh
#!/usr/bin/env ciel

(in-package :ciel-user)
;; lisp code follows.
```

You also need to add the `ciel` binary in your path. A possibility:

    $ ln -s /home/path/to/ciel/bin/ciel ~/.local/bin/ciel

It magically works because before LOAD-ing this Lisp file, we remove the shebang line, and load the remaining Lisp code.

<!-- daaaamn no need of a complex shebang like Roswell like at did at the beginning. See previous commits. Do we want a complex shebang to pass options to the CIEL binary ?

#!/bin/sh
#|-*- mode:lisp -*-|#
#|
exec /path/to/ciel `basename $0` "$@"

How it works:

- it starts as a /bin/sh script
  - all lines starting by `#` are shell comments
- the exec calls the `ciel` binary with this file name as first argument,
  the rest of the file (lisp code) is not read by the shell.
  - before LOAD-ing this Lisp file, we remove the #!/bin/sh shebang line.
  - Lisp ignores comments between `#|` and `|#` and runs the following lisp code.

-->

## Main function and interactive development

TLDR: use the `#+ciel` feature flag as in:

~~~lisp
(in-package :ciel-user)

(defun main ()
  …)

#+ciel
(main)
~~~

Writing scripts is nice, but it is even better when doing so in a
fully interactive Lisp environment, such as in Emacs and Slime (which
is not the only good one anymore ;) ). We then must have a way to have
a piece of code executed when we run the script (the call to the
"main" function doing the side effects), but *not* executed when we
`load` the file or when we compile and load the whole buffer during
development (`C-c C-k`) (note that we can always compile functions
individually with `C-c C-c`).

In Python, the pattern is `__name__ == "__main__"`. In CIEL, we use
Common Lisp's feature flags: the variable `*features*` (inside the
`ciel-user` package) is a list containing symbols that represent
features currently enabled in the Lisp image. For example, here's an
extract:

~~~lisp
CIEL-USER> *features*
(…
 :CL-PPCRE-UNICODE :THREAD-SUPPORT :SWANK :QUICKLISP :ASDF3.3
 :ASDF :OS-UNIX :ASDF-UNICODE :X86-64 :64-BIT
 :COMMON-LISP :ELF :IEEE-FLOATING-POINT :LINUX :LITTLE-ENDIAN
 :PACKAGE-LOCAL-NICKNAMES :SB-LDB :SB-PACKAGE-LOCKS :SB-THREAD :SB-UNICODE
 :SBCL :UNIX)
~~~

Before running your script, we add the `:CIEL` symbol to this
list. The `#+foo` reader macro is the way to check if the feature
"foo" is enabled. You can also use `#-foo` to check its absence. To
always disable a piece of code, the pattern is `#+(or)`, that always
evaluates to nil.

Make sure you are "in" the `ciel-user` package when writing this `#+ciel` check.


## Eval and one-liners

Use `--eval` or `-e` to eval some lisp code.

Example:

```sh
$ ciel -e "(uiop:file-exists-p \"README.org\")"
/home/vindarel/projets/ciel/README.org

$ ciel -e "(-> \"README.org\" (uiop:file-exists-p))"
/home/vindarel/projets/ciel/README.org

$ ciel -e "(-> (http:get \"https://fakestoreapi.com/products/1\") (json:read-json))"

 (dict
  "id" 1
  "title" "Fjallraven - Foldsack No. 1 Backpack, Fits 15 Laptops"
  "price" 109.95
  "description" "Your perfect pack for everyday use and walks in the forest. Stash your laptop (up to 15 inches) in the padded sleeve, your everyday"
  "category" "men's clothing"
  "image" "https://fakestoreapi.com/img/81fPKd-2AYL._AC_SL1500_.jpg"
  "rating"
  (dict
   "rate" 3.9
   "count" 120
  )
 )
```

## Built-in scripts

Call built-in scripts with `--script <scriptname>` or `-s`.

Call `ciel --scripts` to list the available ones.

Those are for demo purposes and are subject to evolve. Ideas and contributions welcome.

### Simple HTTP server

```
$ ciel -s simpleHTTPserver 9000
```

open `http://localhost:9000` and see the list of files.

See `src/scripts/simpleHTTPserver.lisp` in the CIEL repository.

You can preview HTML files and have static assets under a `static/` directory.

Given you have an `index.html` file:

```html
<html>
  <head>
    <title>Hello!</title>
  </head>
  <body>
    <h1>Hello CIEL!</h1>
    <p>
    We just served our own files.
    </p>
  </body>
</html>
```

The script will serve static assets under a `static/` directory.

Now load a .js file as usual in your template:

        <script src="/static/ciel.js"></script>

which can be:

~~~javascript
// ciel.js
alert("hello CIEL!");
~~~

Example output:

```
$ ciel -s simpleHTTPserver 4242
Serving files on port 4242…

  ⤷ http://127.0.0.1:4242

[click on the index.html file]

127.0.0.1 - [2022-12-14 12:06:00] "GET / HTTP/1.1" 200 200 "-" "Mozilla/5.0 (X11; Linux x86_64; rv:103.0) Gecko/20100101 Firefox/103.0"
```

### Quicksearch

Search for Lisp libraries on Quicklisp, Cliki and Github.

see `src/scripts/quicksearch.lisp`.

```lisp
$ ciel -s quicksearch color

SEARCH-RESULTS: "color"
=======================

 Quicklisp
 ---------
  cl-colors
      /home/vince/quicklisp/dists/quicklisp/software/cl-colors-20180328-git/
      http://beta.quicklisp.org/archive/cl-colors/2018-03-28/cl-colors-20180328-git.tgz
      http://quickdocs.org/cl-colors/
[…]
 Cliki
 -----
  colorize
      http://www.cliki.net/colorize
      Colorize is an Application for colorizing chunks of Common Lisp, Scheme,
      Elisp, C, C++, or Java code
[…]
 GitHub
 ------
  colorize
      https://github.com/kingcons/colorize
      A Syntax Highlighting library
  cl-colors
      https://github.com/tpapp/cl-colors
      Simple color library for Common Lisp
[…]
```

### API Pointer

Call a JSON API and access nested data with a JSON pointer:

    ciel -s apipointer URL "/json/pointer"

Example:

    $ ciel -s apipointer https://fakestoreapi.com/products\?limit\=3 "/0/rating/rate"
    3.9

We welcome more capable, expanded scripts!

---

Now, let us iron out the details ;)

### Simple web app with routes

See [`scr/scripts/webapp.lisp`](https://github.com/ciel-lang/CIEL/blob/master/src/scripts/webapp.lisp) for inspiration.

This creates one route on `/` with an optional `name` parameter. Go to `localhost:4567/?name=you` and see.

```lisp
#!/usr/bin/env ciel
;;;
;;; Run with:
;;; $ ./webapp.lisp
;;;

(in-package :ciel-user)

(routes:defroute route-root "/" (&get name)
  (format nil "Hello ~a!" (or name (os:getenv "USER") "lisper")))

(defvar *server* nil)

(defun start-webapp ()
  (setf *server* (make-instance 'routes:easy-routes-acceptor :port 4567))
  (hunchentoot:start *server*))

(defun stop-webapp ()
  (hunchentoot:stop *server*))

#+ciel
(progn
  (start-webapp)
  (format t "~&App started on localhost:4567…~&")
  (sleep most-positive-fixnum))
```

At this point you'll certainly want to live-reload your changes.

### Auto-reload

In this snippet:
[`webapp-notify.lisp`](https://github.com/ciel-lang/CIEL/blob/master/src/scripts/webapp-notify.lisp),
we use the [file-notify](https://github.com/shinmera/file-notify)
library (shipped in CIEL) to watch write changes to our lisp file, and load
it again.

This allows you to have a dumb "live reload" workflow with a simple editor and a terminal.

> WARNING: This does NOT take advantage of Common Lisp's image-based-development features at all. Install yourself a Common Lisp IDE to enjoy the interactive debugger, compiling one function at a time, trying things out in the REPL, autocompletion, code navigation…

> INFO: you need `inotify` on Linux and `fsevent` on MacOS.

~~~lisp
(defun simple-auto-reload ()
  (notify:watch "webapp.lisp")
    (notify:with-events (file change :timeout T)
      ;; Print the available list of events:
      ;; (print (list file change))
      (when (equal change :close-write)
        (format! t "~%~%Reloading ~a…~&" file)
        (handler-case
            (ciel::load-without-shebang "webapp.lisp")
          (reader-error ()
            ;; Catch some READ errors, such as parenthesis not closed, etc. 
            (format! t "~%~%read error, waiting for change…~&"))))))

#+ciel
(unless *server*
  (start-webapp)
  (format t "~&App started on localhost:4567…~&")
  (simple-auto-reload)
  (sleep most-positive-fixnum))
~~~

## Misc

### Load your scripts in the REPL

Calling your scripts from the shell is pretty cool, what if you could
*also* have them available at your fingertips in a Lisp REPL?

TLDR;

```lisp
;; in ~/.cielrc
(ciel::load-without-shebang "~/path/to/yourscript.lisp")
```

As the name suggests, this `load` function works even if your file starts with a shebang line (which is not valid Lisp code, so the default `LOAD` function would fail).

Y'know, sometimes you live longer in a Lisp REPL than in a shell
without noticing. Or simply, manipulating real objects in a text
buffer can be more practical than copy-pasting text in a rigid
terminal (even though Emacs'
[vterm](https://github.com/akermu/emacs-libvterm) is an excellent improvement too).

> INFO: the `~/.cielrc` file is loaded at start-up of the terminal REPL (called with `ciel`), not yet when you start the core image in your IDE.

### Searching files

We use the File Object Finder library
([`fof`](https://gitlab.com/ambrevar/fof), a new library meant to
supersede `find`, `ls`, `stat`, `chown`, `chmod`, `du`, `touch` and
the Osicat library) to search for files recursively:

~~~lisp
(defun find-on-directory (root params)
  (fof:finder*
   :root root
   :predicates (apply #'fof/p:path~ (ensure-list params))))

(find-on-directory "~/Music/" "mp3")
~~~

and this returns a list of `fof:file` objects. Get their real name as
a string with `fof:path`.

Of course, you can also outsource the work to Unix commands, with
`cmd:cmd` (prints to standard output) or `cmd:$cmd` (returns a
string):

~~~lisp
(-> (cmd:$cmd "find . -iname \"*mp3\"")
    str:lines)

;; With find alternative fd:
;; https://github.com/sharkdp/fd
;; apt install fd-find
(-> (cmd:$cmd "fdfind mp3")
    str:lines)

;; Play music:
(cmd:cmd "fdfind mp3 -X mpv")
~~~
