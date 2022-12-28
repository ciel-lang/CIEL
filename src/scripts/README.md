

- it is better to use `uiop:*command-line-arguments*` instead of `(uiop:command-line-arguments)`. The latter always get the original full list, but when calling `ciel -s` we need to pop the arguments list, to ditch the `-s`.
