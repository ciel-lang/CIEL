# CIEL Nix Flake
# A Common Lisp scripting and REPL environment with extended standard library
#
# Development Guide:
# - Run `nix develop` to enter development shell
# - Run `nix run` to start CIEL REPL
# - Run `nix build` to build the package
# - Dependencies are managed via nvfetcher (see nvfetcher.toml)
# - To update dependencies: `nix run nixpkgs#nvfetcher`
# - Non-standard libraries are fetched from GitHub via nvfetcher
{
  description = "A basic flake to with flake-parts";

  inputs = {
    # Use unstable nixpkgs for latest packages
    nixpkgs.url = "github:nixos/nixpkgs/release-25.05"; # Flake-parts for structured flake organization
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = nixpkgs.lib.platforms.all;
      perSystem = { pkgs, lib, ... }:
        let
          ### Package Information ###
          src = ./.; # Source directory for CIEL
          # Python Pygments for syntax highlighting in REPL
          pygments = pkgs.python312Packages.pygments;
          # Native libraries required by CIEL and its dependencies
          nativeLibs = with pkgs;
            [
              asdf
              zstd        # Compression library
              pkgs.python312Packages.pygments    # Syntax highlighting
            ] ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              inotify-tools # File system monitoring (Linux only)
            ];
          # SBCL Common Lisp implementation
          lisp = pkgs.sbcl;
          # Auto-generated sources from nvfetcher for non-standard libraries
          # Run `nix run nixpkgs#nvfetcher` to update
          sources = pkgs.callPackage ./_sources/generated.nix { };
          # Custom build for cl-json-pointer with synonyms system
          # Source managed by nvfetcher (not in Quicklisp)
          _cl-json-pointer-synonyms = lisp.buildASDFSystem {
            pname = "cl-json-pointer";
            version = sources.cl-json-pointer.version;
            systems = [ "cl-json-pointer" "cl-json-pointer/synonyms" ];
            src = sources.cl-json-pointer.src;
            lispLibs = with lisp.pkgs; [ alexandria closer-mop ];
          };
          # Custom build for termp library
          # Source managed by nvfetcher (not in Quicklisp)
          _termp = lisp.buildASDFSystem {
            pname = "termp";
            version = sources.termp.version;
            systems = [ "termp" ];
            src = sources.termp.src;
          };
          # Lisp libraries from nixpkgs and custom builds
          # Most come from Quicklisp, exceptions noted below
          lispLibs = with lisp.pkgs; [
            cl-reexport
            cl-ansi-text
            access
            alexandria
            arrow-macros
            file-finder
            moira
            bordeaux-threads
            trivial-monitored-thread
            lparallel
            cl-cron
            closer-mop
            cl-csv
            cl-csv-data-table
            shasht
            _cl-json-pointer-synonyms
            dissect
            fset
            file-notify
            generic-cl
            dexador
            hunchentoot
            easy-routes
            quri
            lquery
            spinneret
            cl-ftp
            clingon
            local-time
            modf
            parse-float
            parse-number
            dbi
            sxql
            vgplot
            cl-ppcre
            str
            secret-values
            progressons
            _termp
            pythonic-string-reader
            trivia
            trivial-arguments
            trivial-package-local-nicknames
            trivial-types
            metabang-bind
            defstar
            for
            trivial-do
            cmd
            serapeum
            shlex
            function-cache
            fiveam
            which
            log4cl
            printv
            repl-utilities
            named-readtables
            clesh
            quicksearch
            cl-readline
            lisp-critic
            magic-ed
          ];
          ### Package Information End ###
          # Extract version from ciel.asd file and append -git suffix
          version =
            let
              asd = builtins.readFile ./ciel.asd;
              res = builtins.split '':version[[:space:]]*"([^"]*)"'' asd;
              ver = builtins.elemAt (builtins.elemAt res 1) 0;
            in
              "${ver}-git";
          # Main CIEL package build
          ciel = lisp.buildASDFSystem {
            inherit version lispLibs nativeLibs;
            pname = "ciel";
            systems = [ "ciel" "ciel/repl" ]; # Both library and REPL systems
            src = pkgs.stdenv.mkDerivation {
              name = "ciel-patched-src";
              inherit src;
              patches = [ ./patch/remove-quicklisp.patch ];
              dontBuild = true;
              installPhase = "cp -r . $out";
            };
          };
          # SBCL with CIEL package available
          lisp' = lisp.withPackages (ps: [ ciel ]);
          # Pre-built CIEL REPL image for faster startup
          ciel-repl-src = pkgs.writeText "ciel.lisp" ''
            (load (sb-ext:posix-getenv "ASDF"))
            (asdf:load-system :ciel)
            (asdf:load-system :ciel/repl)
            (setf sbcli:*syntax-highlighting* t)
            (setf sbcli::*pygmentize* "${pygments}/bin/pygmentize")
            (sbcli:repl)
          '';
          ciel-repl-bin = pkgs.writeShellScriptBin "ciel" ''
            export LD_LIBRARY_PATH=${lib.makeLibraryPath nativeLibs}
            ${lisp'}/bin/sbcl --script ${ciel-repl-src}
          '';
        in {
          # Overlay for making CIEL available in other flakes
          overlayAttrs = {
            sbcl = pkgs.sbcl.withOverrides (self: super: { inherit ciel; });
            ciel = ciel-repl-bin;
          };
          # Default app: run CIEL REPL with `nix run`
          apps.default = {
            type = "app";
            program = ciel-repl-bin;
          };
          # Development shell: `nix develop` provides CIEL and SBCL
          devShells.default = pkgs.mkShell { packages = [ lisp' ciel-repl-bin ]; };
        };
    };
}
