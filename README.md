
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

<p align="center"><img src="https://avatars.githubusercontent.com/u/72611034?s=48&v=4" /></p>

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
threads and jobs scheduling, for **HTTP** and URI handling,
and so on. You can of course do all this
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

<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-refresh-toc -->
**Table of Contents**

- [CIEL Is an Extended Lisp](#ciel-is-an-extended-lisp)
    - [What is this ?](#what-is-this-)
    - [Rationale](#rationale)
- [Install](#install)
    - [Download a binary. For scripting and the custom REPL.](#download-a-binary-for-scripting-and-the-custom-repl)
- [Build](#build)
    - [Dependencies](#dependencies)
        - [System dependencies](#system-dependencies)
        - [ASDF >= 3.3.4 (local-nicknames)](#asdf--334-local-nicknames)
        - [Install Quicklisp](#install-quicklisp)
        - [Install our Lisp dependencies [depends on your Quicklisp version]](#install-our-lisp-dependencies-depends-on-your-quicklisp-version)
    - [How to load CIEL with Quicklisp](#how-to-load-ciel-with-quicklisp)
    - [How to build a CIEL binary and a core image](#how-to-build-a-ciel-binary-and-a-core-image)
    - [Docker](#docker)
- [Usage](#usage)
    - [Scripting](#scripting)
    - [Terminal REPL](#terminal-repl)
    - [CIEL as a library: "use" :ciel in your Lisp systems](#ciel-as-a-library-use-ciel-in-your-lisp-systems)
    - [Core image: configure your editor](#core-image-configure-your-editor)
- [Libraries](#libraries)
- [Language extensions](#language-extensions)
- [Final words](#final-words)
- [Misc: how to generate the documentation](#misc-how-to-generate-the-documentation)
- [Contributors](#contributors)
- [Lisp?!](#lisp)

<!-- markdown-toc end -->


# Install

## Download a binary. For scripting and the custom REPL.

Getting a binary allows you to run scripts, to play around in its
terminal readline REPL. A binary doesn't allow you to use CIEL in your
existing Common Lisp editor (which still offers the most interactive
and fast development experience).

To download a CIEL binary:

- check our releases on https://github.com/ciel-lang/CIEL/releases/
- we provide a binary from a CI for some systems: go to
  <https://gitlab.com/vindarel/ciel/-/pipelines>, download the latest
  artifacts, unzip the `ciel-v0-{platform}.zip` archive and run `ciel-v0-{platform}/ciel`.

CIEL is currently built for the following platforms:

| Platform | System Version (release date) |
|----------|-------------------------------|
| debian   | Debian Buster (2019)          |
| void     | Void Linux glibc (2023-05), using [cinerion's Docker image](https://github.com/cinerion/sbcl-voidlinux-docker)  |


Start it with `./ciel`.

With no arguments, you enter CIEL's terminal REPL.

You can give a CIEL script as first argument, or call a built-in one. See the scripting section.

# Build

To build CIEL, both the binary and the core image, you need a couple
system dependencies and you have to check a couple things on the side
of lisp before proceeding.

## Dependencies

### System dependencies

You will probably need the following system dependencies (names for a
Debian Bullseye system):

    zlib1g-dev # from deploy for SBCL < 2.2.6

If your SBCL version is >= 2.2.6 you might want to use the more
performant `libzstd-dev` library instead of `zlib1g-dev`.

    libzstd-dev # from deploy for SBCL >= 2.2.6

On Linux:

    inotify-tools

On MacOS:

    fsevent

You can run: `make debian-deps` or `make macos-deps`.


### ASDF >= 3.3.4 (local-nicknames)

ASDF is the de-facto system definition facility of Common Lisp, that
lets you define your system's metadata (author, dependencies, sources,
modules…).

Please ensure that you have ASDF >= 3.3.4. It is for instance not the case with SBCL 2.2.9.

Ask the version with our script:

    $ make check-asdf-version

or yourself with`(asdf:asdf-version)` on a Lisp REPL, or with
this one-liner from a terminal:

    $ sbcl  --eval '(and (print (asdf:asdf-version)) (quit))'

Here's a one-liner to update ASDF:

    $ mkdir ~/common-lisp/
    $ ( cd ~/common-lisp/ && wget https://asdf.common-lisp.dev/archives/asdf-3.3.5.tar.gz  && tar -xvf asdf-3.3.5.tar.gz && mv asdf-3.3.5 asdf )


### Install Quicklisp

To build CIEL on your machine, you need the [Quicklisp library
manager](https://quicklisp.org/beta/). Quicklisp downloads and
installs a library and its dependencies on your machine. It's very
slick, we can install everything from the REPL without restarting our
Lisp process. It follows a "distrubution" approach, think Debian
releases, where libraries are tested to load.

It isn't the only library manager nowadays. See [https://github.com/CodyReichert/awesome-cl#library-manager](https://github.com/CodyReichert/awesome-cl#library-manager).

Install it:

```sh
curl -O https://beta.quicklisp.org/quicklisp.lisp
sbcl --load quicklisp.lisp --eval "(quicklisp-quickstart:install)" --quit
sbcl --load ~/quicklisp/setup.lisp --eval "(ql:add-to-init-file)" --quit
```

It creates a `~/quicklisp/` directory. Read its installation instructions to know more.

### Install our Lisp dependencies [MANDATORY]

One library that we use is not included in Quicklisp (as of
<2025-02-03>), [termp](https://github.com/vindarel/termp). It is a
small and trivial library, you can clone it into your
~/quicklisp/local-projects:

    git clone https://github.com/vindarel/termp/ ~/quicklisp/local-projects/termp

For a number of other libraries we need the Quicklisp version of August, 2024, or later.

For those, you should either:
* ensure that your Quicklisp version is recent enough (with `(ql:dist-version "quicklisp")`) and maybe update it (with `(ql:update-dist "quicklisp")`)
* clone our dependencies locally with the command below.

If you need it, clone all the required dependencies into your `~/quicklisp/local-projects/` with this command:

    make ql-deps

NB: other tools exist for this (Qlot, ocicl…), we are just not using them yet.


## How to load CIEL with Quicklisp

You need the dependencies above: Quicklisp, a good ASDF version, our up-to-date Lisp dependencies.

This shows you how to load CIEL and all its goodies, in order to use it in your current editor.

CIEL is not on Quicklisp yet, but it is on [Ultralisp](https://ultralisp.org).

So, either clone this repository:

    git clone https://github.com/ciel-lang/CIEL ~/quicklisp/local-projects/CIEL

or install the Ultralisp distribution and pull the library from there:

~~~lisp
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
~~~

Now, in both cases, you can load the `ciel.asd` file (with `asdf:load-asd`
or `C-c C-k` in Slime) and quickload "ciel":

```lisp
CL-USER> (ql:quickload "ciel")
```

be sure to enter the `ciel-user` package:

```lisp
(in-package :ciel-user)
```
you now have access to all CIEL's packages and functions.


## How to build a CIEL binary and a core image

You need the dependencies above: Quicklisp, a good ASDF version, our up-to-date Lisp dependencies.

To build CIEL's binary, use:

    $ make build

This creates a `ciel` binary in the current directory.

To create a Lisp image:

    $ make image
    # or
    $ sbcl --load build-image.lisp

This creates the `ciel-core` Lisp image.

Unlike a binary, we can not distribute core images. It is dependent on the machine it was built on.

The way we use a core image is to load it at startup like this:

    sbcl --core ciel-core --eval '(in-package :ciel-user)'

It loads fast and you have all CIEL libraries and goodies at your disposal.

Then you have to configure your editor, like Slime, to have the choice of the Lisp image to
start. See below.


## Docker

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
  - including for files (after a bracket) and binaries in the PATH.
- it handles errors gracefully: you are not dropped into the debugger
  and its sub-REPL, you simply see the error message.
- it has optional **syntax highlighting**.
- it has a **shell pass-through**: try `!ls`.
  - it runs **interactive commands**: try `!htop`, `!vim test.lisp`, `!emacs -nw test.lisp` or `!env FOO=BAR sudo -i top`.
- it has **documentation lookup** shorthands: use `:doc symbol` or `?`
  after a symbol to get its documentation: `ciel-user> (dict ?`.
- it has **developer friendly** macros: use `(printv code)` for an
  annotated trace output.
- it has an optional **lisp critic** that scans the code you enter at
  the REPL for instances of bad practices.
- and it defines some more helper commands.
- it works on Slime (to a certain extent)

The CIEL terminal REPL loads the `~/.cielrc` init file at start-up if present. Don't load it with `--no-userinit`.

See more in [*the documentation*](https://ciel-lang.github.io/CIEL/#/repl).

> [!NOTE]
> Our terminal readline REPL does NOT replace a good Common Lisp editor. You have more choices than Emacs. Check them out! https://lispcookbook.github.io/cl-cookbook/editor-support.html


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


## CIEL as a library: "use" :ciel in your Lisp systems

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

## Core image: configure your editor

The advantage of a core image is that it loads instantly, faster than
a `(ql:quickload "ciel")`. We'll ask our editor to start SBCL with our
CIEL core image.

We'll configure SLIME for [multiple Lisps](https://common-lisp.net/project/slime/doc/html/Multiple-Lisps.html#Multiple-Lisps).

You need to add this to your Emacs init file:

```lisp
(setq slime-lisp-implementations
      `((sbcl    ("sbcl" "--dynamic-space-size" "2000"))  ;; default. Adapt if needed.
        (ciel-sbcl  ("sbcl" "--core" "/path/to/ciel/ciel-core" "--eval" "(in-package :ciel-user)"))))
(setq slime-default-lisp 'ciel-sbcl)
```

and start a Lisp process with `M-x slime`.

If you didn't set `ciel-sbcl` as the default, then start the Lisp
process with `M-- M-x slime` (alt-minus prefix), and choose
`ciel-sbcl`. You can start more than one Lisp process from SLIME.

The Lisp process should start instantly, as fast as the default SBCL,
you won't wait for the quicklisp libraries to load.


# Libraries

We import, use and document libraries to fill various use cases: generic
access to data structures, functional data structures, string
manipulation, JSON, database access, web, URI handling, iteration
helpers, type checking helpers, syntax extensions, developer utilities,
etc.

See the documentation.

To see the full list of dependencies, see the `ciel.asd` project
definition or this [dependencies list](docs/dependencies.md).

# Language extensions

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
- 🎥 [Youtube showcases](https://www.youtube.com/@vindarel):
  - [Debugging Lisp: fix and resume a program from any point in the stack](https://www.youtube.com/watch?v=jBBS4FeY7XM)
  - [How to call a REST API in Common Lisp: HTTP requests, JSON parsing, CLI arguments, binaries](https://www.youtube.com/watch?v=TAtwcBh1QLg)
- 🎥 my [Common Lisp course in videos: from novice to efficient programmer](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358), on the Udemy platform.
