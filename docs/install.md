
# Install

CIEL can be used in two different roles:

- As a library that you can load into any Common Lisp implementation.
- As a binary based on `sbcl`, which  is a command-line tool for use in the terminal or in shell scripts. It provides a more feature-rich REPL than standard `sbcl`, and a much faster startup time than starting `sbcl` and then loading the CIEL library.

If you use a Lisp development environment, such as Emacs with Slime, you should opt for the library rather than the binary. To get the same fast startup time, you can use a prebuilt core image, as we will explain below.

In the following, we will explain how to install the library, the binary, and the prebuilt core image, for various common SBCL setups.

## Download a prebuilt binary.

To download a CIEL binary:

- check our releases on https://github.com/ciel-lang/CIEL/releases/
- we provide a binary from a CI for some systems: go to
  <https://gitlab.com/vindarel/ciel/-/pipelines>, download the latest
  artifacts, unzip the `ciel-v0-{platform}.zip` archive and run `ciel-v0-{platform}/ciel`.
- if you use the [Guix](https://guix.gnu.org/) package manager, install package `sbcl-ciel-repl`.
- if you use the [Nix](https://nixos.org/) package manager in Linux and macOS, install package `ciel` and `ciel` in SBCL.

CIEL is currently built for the following platforms:

| Platform | System Version (release date) |
|----------|-------------------------------|
| debian   | Debian Buster (2019)          |
| void     | Void Linux glibc (2023-05), using [cinerion's Docker image](https://github.com/cinerion/sbcl-voidlinux-docker)  |

Start it with `./ciel` (adapt the path if you put the binary somewhere else).

With no arguments, you enter CIEL's terminal REPL.

You can give a CIEL script as first argument, or call a built-in one. See the scripting section.

## Run the binary in a Docker container

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

## Install CIEL as a library

You can install and CIEL like any other Common Lisp library, but you must make sure to also get all of its dependencies, a task that is best left to a package manager.

### Quicklisp

CIEL is not on Quicklisp yet, but it is on [Ultralisp](https://ultralisp.org).

So, either clone this repository:

    git clone https://github.com/ciel-lang/CIEL ~/quicklisp/local-projects/CIEL

or install the Ultralisp distribution and pull the library from there:

```lisp
(ql-dist:install-dist "http://dist.ultralisp.org/" :prompt nil)
```

#### Install our Lisp dependencies [MANDATORY]

Even if you have a Lisp setup with Quicklisp installed, the current
distribution of Quicklisp is quite old (as of August, 2024) and you
need to pull recent dependencies.

We'll clone the required ones into your `~/quicklisp/local-projects/`.

    make ql-deps

Other tools exist for this (Qlot, ocicl…), we are just not using them yet.

#### Loading CIEL with Quicklisp

Now, in both cases, you can load the `ciel.asd` file (with `asdf:load-asd`
or `C-c C-k` in Slime) or quickload "ciel":

```lisp
(ql:quickload "ciel")
```

be sure to enter the `ciel-user` package:

```lisp
(in-package :ciel-user)
```
you now have access to all CIEL's packages and functions.

### Guix

CIEL is available via the [Guix](https://guix.gnu.org/) package manager, as a source code package (`cl-ciel`) or precompiled for SBCL (`sbcl-ciel`) and ECL (`ecl-ciel`). You have to add Lisp itself (package `sbcl` or `ecl`), and any other Lisp library you may want to use.

In Lisp, do
```lisp
(require "asdf")
(asdf:load-system :ciel)
(in-package :ciel-user)
```

Alternatively, or in addition, you can install `sbcl-ciel:image`, which contains a prebuilt core image under `bin/ciel.image`. It is executable, so you can run it in place of `sbcl`, or you can load it from the `sbcl` command line:

```
sbcl --core $(GUIX_PROFILE)/bin/ciel.image
```

In either case, you get a Lisp environment with CIEL preloaded, so all you have to do is

```lisp
(in-package :ciel-user)
```

### Nix

CIEL's repository contains a `flake.nix` file that can be used to build CIEL with Nix.
To use it, you need to add the CIEL repository to your `flake.nix` file.

#### Install CIEL with home-manager

You can use CIEL with home-manager by adding the CIEL repository to your `home.nix` file.

1. Add the CIEL repository to your `flake.nix` file.
2. Add the CIEL overlay to your Nixpkgs overlay list.
3. Add CIEL to your `home.packages` list.

```diff
# flake.nix
inputs = {
+  # Add the CIEL repository
+  ciel.url = "github:ciel-lang/CIEL";
+  ciel.inputs.nixpkgs.follows = "nixpkgs";
};
outputs = { self, ciel, nixpkgs, ... }:
  flake-utils.lib.eachDefaultSystem (system: {
  legacyPackages.homeConfigurations."USERNAME" = home-manager.lib.homeManagerConfiguration {
    pkgs = import nixpkgs {
      inherit system;
      overlays = [
+        # Add the CIEL overlay
+        ciel.overlays.default
      ];
    };
```

```diff
# home.nix
packages = with pkgs; [
+  # Use CIEL command line interface
+  ciel
+  # Use CIEL as a library
+  sbcl.withPackages (ps: [ ps.ciel ]);
];
```

#### Install CIEL to your devShell

You need to modify your `flake.nix` file to include CIEL in your development environment.

1. Add the CIEL repository to your `flake.nix` file.
2. Add the CIEL overlay to your Nixpkgs overlay list.
3. Add CIEL to your `devShell` packages or any other package set.

```diff
{
  inputs = {
+    # Add the CIEL repository
+    ciel.url = "github:ciel-lang/CIEL";
+    ciel.inputs.nixpkgs.follows = "nixpkgs";
  };
  outputs = { self, ciel, nixpkgs }:
    let
      pkgs = import nixpkgs {
        inherit system;
        overlays = [
+          # Add the CIEL overlay
+          ciel.overlays.default
        ];
      };
    in
    {
      devShell = nixpkgs.mkShell {
        packages = with pkgs; [
+          # Use CIEL command line interface
+          ciel
+          # Use CIEL as a library
+          sbcl.withPackages (ps: [ ps.ciel ]);
        ];
      };
    };
}
```

## Using CIEL as a library in your Lisp code

To use it in your project, create a package and "use" `ciel` in addition
to `cl`:

```lisp
(defpackage yourpackage
  (:use :cl :ciel))
```

Alternatively, you can use `generic-ciel`, based on
[generic-cl](https://github.com/alex-gutev/generic-cl/) (warn:
generic-ciel is less tested at the moment).

```lisp
(defpackage yourpackage
  (:use :cl :generic-ciel))
```

`generic-cl` allows you to define `+` or `equalp` methods for your own
objects (and more).

# Building CIEL binaries and core images

To build CIEL, both the binary and the core image, you need a couple
system dependencies and you have to check a couple things on the side
of Lisp before proceeding.

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

See the section above for loading CIEL via Quicklisp for how to make sure you have all the required dependencies.

### Run the build procedure

You need the dependencies above: Quicklisp, a good ASDF version, our up-to-date Lisp dependencies.

To build CIEL's binary, use:

    $ make build

This creates a `ciel` binary in the current directory.

To create a Lisp image:

    $ make image
    # or
    $ sbcl --load build-image.lisp

This creates the `ciel-core` Lisp image.

Unlike binaries, we cannot distribute core images. They depend on the machine they was were built on.

### Using the core image

The way we use a core image is to load it at startup like this:

    sbcl --core ciel-core --eval '(in-package :ciel-user)'

It loads fast and you have all CIEL libraries and goodies at your disposal.

Then you have to configure your editor, like Slime, to have the choice of the Lisp image to
start. See below.

### Core image: configure your editor

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
