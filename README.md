
<p>
  <h2 align="center"> CIEL </h2>
  <h3 align="center"> Common Lisp, batteries included. </h3>
</p>

<p align="center">
  <a href="https://ciel-lang.github.io/CIEL/#/"><b> Home page </b></a> |
  <a href="https://github.com/ciel-lang/CIEL/issues"><b>Issues</b></a> |
  <a href="https://github.com/ciel-lang/CIEL/discussions"><b> Discussions</b></a> |
  <a href="https://github.com/sponsors/vindarel"> Support us! </a> |
  <a href="https://ko-fi.com/vindarel"> Buy me a coffee! </a>
</p>

# CIEL Is an Extended Lisp

STATUS: ~~highly~~ WIP, the API WILL change, but it is usable.

I am dogfooding it in public and private projects.


## What is this ?

CIEL is a ready-to-use collection of libraries.

It's Common Lisp, batteries included.

It comes in 3 forms:

- a binary, to run CIEL **scripts**: fast start-up times, standalone image, built-in utilities.
- a simple full-featured **REPL** for the terminal.
- a **Lisp library** and a **core image**.

Questions, doubts? See the [FAQ](docs/FAQ.md).

NEW: we now have a Docker file.

```lisp
#!/usr/bin/env ciel

(-> "https://fakestoreapi.com/products?limit=5"
  http:get
  json:read-json
  (elt 0)
  (access "title"))
```

```bash
$ chmodx +x getproduct.lisp
$ time ./getproduct.lisp
"Fjallraven - Foldsack No…ckpack, Fits 15 Laptops"
./getproduct.lisp  0.10s user 0.02s system 24% cpu 0.466 total
```


## Rationale

One of our goals is to make Common Lisp useful out of the box for
mundane tasks -by today standards. As such, we ship libraries to handle
**JSON** or **CSV**, as well as others to ease string
manipulation, to do pattern matching, to bring regular expressions, for
threads and jobs scheduling, for **HTTP** and URI handling, to
create simple GUIs with nodgui (Tk-based, nice theme), and so on. You can of course do all this
without CIEL, but then you have to install the library manager first and
load these libraries into your Lisp image every time you start it. Now,
you have them at your fingertips whenever you start CIEL.

We also aim to soften the irritating parts of standard Common Lisp. A
famous one, puzzling for beginners and non-optimal for seasoned lispers,
is the creation of hash-tables. We include the `dict` function from the
Serapeum library (which we enhanced further with a pull request):

    CIEL-USER> (dict :a 1 :b 2 :c 3)

which prints:

``` txt
(dict
 :A 1
 :B 2
 :C 3
)
```

In standard Common Lisp, the equivalent is more convoluted:

``` commonlisp
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    ht)
;; #<HASH-TABLE :TEST EQUAL :COUNT 3 {1006CE5613}>
;; (and we don't get a readable representation, so our example is not even equivalent)
```

Moreover, we bring:

-   a **full featured REPL on the terminal** and
-   **scripting capabilities**, see more below.

See *the documentation*.

# Install

You will probably need the following system dependencies (names for a
Debian Bullseye system):

    libmagic-dev libc6-dev gcc  # from magicffi
    zlib1g-dev # from deploy for SBCL < 2.2.6

If your SBCL version is >= 2.2.6 you might want to use the more
performant `libzstd-dev` library instead of `zlib1g-dev`.

    libzstd-dev # from deploy for SBCL >= 2.2.6

On Linux:

    inotify-tools

On MacOS:

    fsevent


## With Quicklisp

You need a Lisp implementation and Quicklisp installed.

You need the system dependencies above.

You need a CL implementation with a recent enough version of ASDF to support package-local nicknames. As of March, 2023, this is not the case with SBCL 2.2.9. Here's a one-liner to update ASDF:

    $ ( cd ~/common-lisp/ && wget https://asdf.common-lisp.dev/archives/asdf-3.3.5.tar.gz  && tar -xvf asdf-3.3.5.tar.gz && mv asdf-3.3.5 asdf )

CIEL is not on Quicklisp yet, but it is on [Ultralisp](https://ultralisp.org).

So, either clone this repository:

    git clone https://github.com/ciel-lang/CIEL ~/quicklisp/local-projects/CIEL

And install dependencies missing or outdated from Quicklisp:

    $ ( cd ~/quicklisp/local-projects/CIEL && make ql-deps )

Or install the Ultralisp distribution and pull the library from
there:

    (ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)

Then, load the .asd file (with `asdf:load-asd` or `C-c C-k` in Slime),
quickload "ciel":

```lisp
(ql:quickload "ciel")
```

and enter the `ciel-user` package:

```lisp
(in-package :ciel-user)
```

To build CIEL's binary and core image, use

    $ make build

This creates the `bin/` directory with the `ciel` binary.

    $ make image

This creates the `ciel-core` Lisp image.


## With a core image

You need a Lisp implementation, but you don't need Quicklisp.

Build a *core image* for your lisp with all CIEL's dependencies:

    sbcl --load build-image.lisp

and use it:

    sbcl --core ciel-core --eval '(in-package :ciel-user)'

Then you can configure Slime to have the choice of the Lisp image to
start. See below in *\*Use CIEL at startup*

We ~~will distribute ready-to-use core images~~ can not distribute core
images, you must build it yourself.

## With a binary. Use CIEL's custom REPL.

You don't need anything, just download the CIEL executable and run it.
You need to build the core image yourself though.

- we provide an experimental binary for some systems: go to
  <https://gitlab.com/vindarel/ciel/-/pipelines>, download the latest
  artifacts, unzip the `ciel-v0-{platform}.zip` archive and run `ciel-v0-{platform}/ciel`.

CIEL is currently built for the following platforms:

| Platform | System Version (release date) |
|----------|-------------------------------|
| debian   | Debian Buster (2019)          |
| void     | Void Linux glibc (2023-05)    |


To build it, clone this repository and run `make build`.

Start it with `./ciel`.

## With Docker

We have a Dockerfile.

Build your CIEL image:

    docker build -t ciel .

The executable is built in `/usr/local/bin/ciel` of the Docker image.

Get a CIEL REPL:

    docker run --rm -it ciel /usr/local/bin/ciel

Run a script on your filesystem:

    docker run --rm -it ciel /usr/local/bin/ciel path/to/your/lisp/script.lisp

Run a built-in script:

    docker run --rm -it ciel /usr/local/bin/ciel -s simpleHTTPserver

So, save you some typing with a shell alias:

    alias ciel="sudo docker run --rm -it ciel /usr/local/bin/ciel"


# Usage

## Scripting

> [!NOTE]
> this is brand new! Expect limitations and changes.

Get the `ciel` binary and call it with your .lisp script:

    $ ciel script.lisp

Use the `#!/usr/bin/env ciel` shebang line to directly call your files:

    $ ./script

Call built-in scripts:

    $ ciel -s simpleHTTPserver 9000

See available built-in scripts with `--scripts`.

See [the scripts documentation](https://ciel-lang.github.io/CIEL/#/scripting).

## Terminal REPL

CIEL ships a terminal REPL for the terminal which is more user friendly than the default SBCL one:

- it has readline capabilities, meaning that the arrow keys work by
  default (woohoo!) and there is a persistent history, like in any
  shell.
- it has **multiline input**.
- it has **TAB completion**.
- it handles errors gracefully: you are not dropped into the debugger
  and its sub-REPL, you simply see the error message.
- it has optional **syntax highlighting**.
- it has an optional **lisp critic** that scans the code you enter at
  the REPL for instances of bad practices.
- it has a **shell pass-through**: try `!ls`.
- it has **documentation lookup** shorthands: use `:doc symbol` or `?`
  after a symbol to get its documentation: `ciel-user> (dict ?`.
- it has **developer friendly** macros: use `(printv code)` for an
  annotated trace output.
- it integrates the **lisp critic**.
- and it defines some more helper commands.

The CIEL terminal REPL loads the `~/.cielrc` init file at start-up if present. Don't load it with `--no-userinit`.

See more in [*the documentation*](https://ciel-lang.github.io/CIEL/#/).


Run `ciel` with no arguments:

```bash
$ ciel

CIEL's REPL version 0.1.5
Read more on packages with readme or summary. For example: (summary :str)
Special commands:
  %help => Prints this general help message
  %doc => Print the available documentation for this symbol.
  %? => Gets help on a symbol <sym>: :? str
  %w => Writes the current session to a file <filename>
  %d => Dumps the disassembly of a symbol <sym>
  %t => Prints the type of a expression <expr>
  %q => Ends the session.
  %lisp-critic => Enable or disable the lisp critic. He critizes the code you type before compiling it.
  %edit => Edit a file with EDITOR and evaluate it.
Press CTRL-D or type :q to exit

ciel-user>
```

It is freely based on [sbcli](https://github.com/hellerve/sbcli).


## Lisp library

You can install and `quickload` CIEL like any other Common Lisp library.

To use it in your project, create a package and "use" `ciel` in addition
of `cl`:

```lisp
(defpackage yourpackage
  (:use :cl :ciel))
```

You can also use `generic-ciel`, based on
[generic-cl](https://github.com/alex-gutev/generic-cl/) (warn:
generic-ciel is less tested at the moment).

~~~lisp
    (defpackage yourpackage
      (:use :cl :generic-ciel))
~~~

generic-cl allows us to define our `+` or `equalp` methods for our own
objects (and more).

## Core image: use CIEL in your current developer setup

You can enter the `CIEL-USER` package when you start your Lisp image
from your editor.

A working, but naive and slow-ish approach would be to add this in your
`~/.sbclrc`:

```lisp
(ql:quickload "ciel")
(in-package :ciel-user)
(ciel-user-help)
```

A faster way is to use CIEL's core image and to use SLIME's or your
editor's feature to [configure multiple
Lisps](https://common-lisp.net/project/slime/doc/html/Multiple-Lisps.html#Multiple-Lisps).

You need to:

- build CIEL's core image for your machine (`make image`),
- add this to your Emacs init file:

```lisp
(setq slime-lisp-implementations
      `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))  ;; default. Adapt if needed.
        (ciel-sbcl  ("sbcl" "--core" "/path/to/ciel/ciel-core" "--eval" "(in-package :ciel-user)"))))
(setq slime-default-lisp 'ciel-sbcl)
```

- and start a new Lisp process.
- optional: if you didn't set it as default with `slime-default-lisp`,
  then start a new Lisp process with `M-- M-x slime` (alt-minus
  prefix), and choose ciel-sbcl. You can start more than one Lisp
  process from SLIME.

The Lisp process should start instantly, as fast as the default SBCL,
you won't wait for the quicklisp libraries to load.

## Libraries

We import, use and document libraries to fill various use cases: generic
access to data structures, functional data structures, string
manipulation, JSON, database access, web, URI handling, GUI, iteration
helpers, type checking helpers, syntax extensions, developer utilities,
etc.

See the documentation.

To see the full list of dependencies, see the `ciel.asd` project
definition or this [dependencies list](docs/dependencies.md).

## Language extensions

We provide arrow macros, easy type declaratons in the function lambda
list, macros for exhaustiveness type checking, pattern matching, etc.

See [the documentation](https://ciel-lang.github.io/CIEL/#/language-extensions).

# Final words

That was your life in CL:

<p align="center"><img src="docs/before.jpeg" /></p>

and now:

<p align="center"><img src="docs/after-plus.jpeg" /></p>

# Misc: how to generate the documentation

See `src/ciel.lisp` and run `(generate-dependencies-page-reference)`.

# Contributors

Special big thanks to @cinerion, [@themarcelor](https://github.com/themarcelor) and everyone who helped (@agam, @patrixl, @bo-tato…).


# Lisp?!

- [awesome-cl](https://github.com/CodyReichert/awesome-cl)
- [the Common Lisp Cookbook](https://lispcookbook.github.io/cl-cookbook/)
  - [editor support](https://lispcookbook.github.io/cl-cookbook/editor-support.html) (Emacs, Vim, VSCode, Atom, Pulsar, Jetbrains, Sublime, Jupyter notebooks…)
- [Lisp companies](https://github.com/azzamsa/awesome-lisp-companies/)
- blog posts:
  - [these years in Lisp: 2022 in review](https://lisp-journey.gitlab.io/blog/these-years-in-common-lisp-2022-in-review/)
  - [Python VS Common Lisp, workflow and ecosystem](https://lisp-journey.gitlab.io/pythonvslisp/)
  - [A road to Common Lisp](https://stevelosh.com/blog/2018/08/a-road-to-common-lisp/)
- 🎥 my [Common Lisp course in videos: from novice to efficient programmer](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358), on the Udemy platform.
