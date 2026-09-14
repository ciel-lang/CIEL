
# 0.3 - 2026-09-12

- added `yamson` YAML parser
- added `periods`, wrote and published its documentation.
- added `defclass-std` (from https://github.com/lisp-maintainers/defclass-std but Quicklisp points to the original one)
- set `*read-default-float-format` to `'double-float`

# 2026-06

- build a binary from the project at point: `ciel -s build …`
- build a script as a CIEL binary: `ciel -s embed script.lisp`
- install Quicklisp the classical way (no HTTPS): `ciel -s install-raw-quicklisp`

removed:

- ~~install Quicklisp with HTTPS: `ciel -s install-quicklisp`~~
- ~~install a Quicklisp library: `ciel -s install …`~~

just use [ql-https](https://github.com/rudolfochrist/ql-https/)'s one-liner and Qlot or ocicl.

# 2025

- add CSV libraries
- aliases: `nappend`, `nremove`
- example script: transform music files to .mp3 with `ffmpeg`
- post-installation option: ZSH tab completion

# < 2025

- first releases
