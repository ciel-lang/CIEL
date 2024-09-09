
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
- using the [Guix](https://guix.gnu.org/) package manager, install package `sbcl-ciel-repl`.

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

Please ensure that you have ASDF >= 3.3.4.

It is for instance not the case with SBCL 2.2.9.

Ask the version with `(asdf:asdf-version)` on a Lisp REPL, or with
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

Even if you have a Lisp setup with Quicklisp installed, the current
distribution of Quicklisp is quite old (as of August, 2024) and you
need to pull recent dependencies.

We'll clone the required ones into your `~/quicklisp/local-projects/`.

    make ql-deps

Other tools exist for this (Qlot, ocicl…), we are just not using them yet.


## How to load CIEL with Quicklisp

You need the dependencies above: Quicklisp, a good ASDF version, our up-to-date Lisp dependencies.

This shows you how to load CIEL and all its goodies, in order to use it in your current editor.

CIEL is not on Quicklisp yet, but it is on [Ultralisp](https://ultralisp.org).

So, either clone this repository:

    git clone https://github.com/ciel-lang/CIEL ~/quicklisp/local-projects/CIEL

or install the Ultralisp distribution and pull the library from there:

```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
```

Now, in both cases, you can load the `ciel.asd` file (with `asdf:load-asd`
or `C-c C-k` in Slime) and quickload "ciel":

```lisp
(ql:quickload "ciel")
```

be sure to enter the `ciel-user` package:

```lisp
(in-package :ciel-user)
```
you now have access to all CIEL's packages and functions.


## How to build a CIEL binary and a core image

If you use the [Guix](https://guix.gnu.org/) package manager, install package output `sbcl-ciel:image`. It contains a prebuilt image under `bin/ciel.image`.

For all other setups, you have to build a core image yourself. You need the dependencies above: Quicklisp, a good ASDF version, our up-to-date Lisp dependencies.

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

# Usage as a library

## "use" ciel in your Lisp systems

You can install and `quickload` CIEL like any other Common Lisp library. It is also available via the [Guix](https://guix.gnu.org/) package manager, as a source code package (`cl-ciel`) or precompiled for SBCL (`sbcl-ciel`) and ECL (`ecl-ciel`).

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
