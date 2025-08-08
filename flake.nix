{
  description = "A basic flake to with flake-parts";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs?ref=nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
  };

  outputs = inputs@{ nixpkgs, flake-parts, ... }:

    flake-parts.lib.mkFlake { inherit inputs; } {
      imports = [ flake-parts.flakeModules.easyOverlay ];
      systems = nixpkgs.lib.platforms.all;
      perSystem = { pkgs, lib, ... }:
        let
          ### Package Information ###
          src = ./.;
          pygments = pkgs.python312Packages.pygments;
          nativeLibs = with pkgs;
            [
              asdf
              zstd
              pygments
            ] ++ lib.optionals pkgs.stdenv.hostPlatform.isLinux [
              inotify-tools
            ];
          lisp = pkgs.sbcl;
          lispLibs = with lisp.pkgs; [
            ## (asdf:system-depends-on (asdf:find-system "ciel"))
            ### Exception:
            ### cl-json-pointer/synonyms -> cl-json-pointer-with-synonyms
            ### moira/light -> moira
            ### Note:
            ### termp and cl-json-pointer-with-synonyms are not in Quicklisp and are fetched from GitHub
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
            cl-ansi-text
            cl-csv
            shasht
            cl-json-pointer-synonyms
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
            termp
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
            fiveam
            which
            log4cl
            printv
            repl-utilities
            named-readtables
            clesh
            quicksearch
            ## (asdf:system-depends-on (asdf:find-system "ciel/repl"))
            cl-readline
            lisp-critic
            magic-ed
          ];
          ### Package Information End ###
          version =
            let
              asd = builtins.readFile ./ciel.asd;
              res = builtins.split '':version[[:space:]]*"([^"]*)"'' asd;
              ver = builtins.elemAt (builtins.elemAt res 1) 0;
            in
              "${ver}-git";
          cl-json-pointer-synonyms = lisp.buildASDFSystem {
            pname = "cl-json-pointer";
            version = "20221106-git";
            systems = [ "cl-json-pointer" "cl-json-pointer/synonyms" ];
            src = builtins.fetchGit {
              url = "https://github.com/y2q-actionman/cl-json-pointer";
              rev = "f6760e2a02972783f96b92a15f801e14a6828e0c";
            };
            lispLibs = with lisp.pkgs; [ alexandria closer-mop ];
          };
          termp = lisp.buildASDFSystem {
            pname = "termp";
            version = "20241103-git";
            systems = [ "termp" ];
            src = builtins.fetchGit {
              url = "https://github.com/vindarel/termp";
              rev = "29789fe83db624679b6f341e3fae3f2577ce6a45";
            };
          };
          ciel = lisp.buildASDFSystem {
            inherit version src lispLibs nativeLibs;
            pname = "ciel";
            systems = [ "ciel" "ciel/repl" ];
          };
          lisp' = lisp.withPackages (ps: [ ciel ]) // { inherit (lisp) meta; };
          ciel-repl-image = pkgs.stdenv.mkDerivation {
            pname = "ciel-repl";
            inherit version src;
            nativeBuildInputs = [ lisp' pkgs.asdf ];
            buildInputs = [ pygments ];
            buildPhase = "# no build phase";
            installPhase = ''
              mkdir -p $out
              export LD_LIBRARY_PATH=${lib.makeLibraryPath nativeLibs}
              ${lisp'}/bin/${lisp'.meta.mainProgram} --noinform <<EOF
                (load (sb-ext:posix-getenv "ASDF"))
                (asdf:load-system :ciel)
                (asdf:load-system :ciel/repl)
                (setf sbcli:*syntax-highlighting* t)
                (setf sbcli::*pygmentize* "${pygments}/bin/pygmentize")
                (uiop:dump-image "$out/ciel")
              EOF
            '';
          };
          ciel-repl = pkgs.writeShellScriptBin "ciel" ''
            export LD_LIBRARY_PATH=${lib.makeLibraryPath nativeLibs}
            exec ${lisp'}/bin/${lisp'.meta.mainProgram} --noinform \
              --core '${ciel-repl-image}/ciel' \
              --eval '(ciel::main)' \
              "$@"
          '';
        in {
          overlayAttrs = {
            sbcl = pkgs.sbcl.withOverrides (self: super: { inherit ciel; });
            ciel = ciel-repl;
          };
          apps.default = {
            type = "app";
            program = ciel-repl;
          };
          devShells.default = pkgs.mkShell { packages = [ ciel-repl lisp' ]; };
        };
    };
}
