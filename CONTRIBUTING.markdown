
Thanks for contributing to CIEL. We hope it is useful to you and will
be now more useful to everybody.

Please follow these short guidelines. They'll help the maintainer(s)
craft release notes and they make for a clearer commits log, *IMO*. Thank you!

## Commit messages

Please say at the beginning what your commit is changing: is it about
dependencies? The Makefile, the .asd? The terminal/readline interface?

` README:` for the README
- `docs:` is for documentation
- `deps:` for the Lisp dependencies (be more explicit for system-wide dependencies)
- `CI:`
- `asd:`
- `terminal`: for the terminal REPL

Example:

> deps: libmagic-dev is no more required

If your change is about a domain, you can say it up front too. For example:

> database: mention the need of db drivers for binaries

If your change is adding or removing something, you can say this action up front.

If your change is wider or doesn't fit here, don't think harder, just contribute. Thanks.


### Minor commit messages

I like to see the `(minor)` mention when the change is really trivial
and not worth looking at. Likewise, we can grep-it out from the
release notes.

For example:

> (minor) add site icon

> (minor) make run typo

## Avoid small and useless commits, squash them

Please avoid small commits that say "fix" "fix" and again
"fix". Squash them into one with a good commit message (see above),
thank you.
