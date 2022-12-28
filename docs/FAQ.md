About CIEL
==========

Is CIEL yet another language re-design?
---------------------------------------

Absolutely not. CIEL is plain Common Lisp. We don't redefine the semantics of the language. CIEL is a collection of useful libraries, shipped as one Quicklisp meta-library, a core image and an executable.

Is CIEL a standard library?
---------------------------

No, we can't say that. We ship useful libraries written by a variety of people, including ourselves, and we make them available to you so you don't have to spend time looking for them, choosing them, installing them and importing them. We use the same libraries that every other lisper can find on Quicklisp. We provide a core image and a ready-to-use REPL to make on-boarding even easier.

If I use CIEL, do I learn Common Lisp?
--------------------------------------

Yes you do. And in addition you'll be acquainted to useful and often popular third-party libraries and utilities. They are all referenced on our website.

How do I switch from CIEL to plain Common Lisp?
-----------------------------------------------

You will have to know what external libraries you are using. Mainly, by reading CIEL's documentation, or by using your editor's "go to definition" feature (`M-.` in Slime) to find out. You'll have to use the `cl` or `cl-user` package instead of `ciel-user`, and you'll have to declare your dependencies yourself in your project's `.asd` file.

Eventually, there could be a script that does that for you.

Is CIEL as fast as Common Lisp?
-------------------------------

In general, yes. Only some functions are more generic, thus slower, than default Common Lisp. For example, `access`. That is more the case if you use `generic-ciel`.

Is CIEL stable?
---------------

No. At least not in the Common Lisp sense of stability, which means "very stable". CIEL is as stable as the set of the libraries it includes. We have solutions to improve stability, so this is an open question (using our own Quicklisp distribution, redifining a symbol in case of an upstream change for backwards compatibility,…).

Generally though, the ecosystem is quite conservative. We saw deprecation warnings staying for 12 years.

Who is CIEL for?
----------------

CIEL is for everybody who wants to discover Common Lisp, or for more experienced lispers who want to have a batteries-included Common Lisp distribution at hand.

I am a seasoned lisper, why should I care?
------------------------------------------

You must regularly hear that "getting started with Common Lisp is hard", "Common Lisp is full of quirks", "finding what one needs is difficult", etc. CIEL is an attempt to ease on-boarding, and getting rid off these (legitimate) complaints.

You can test and discover new libraries.

You can show CIEL to your non-lispers friends and colleagues, without saying embarrassing things like: "Just install rlwrap". "To join strings, use format's ~{ @ : }". "Yeah, there's parse-integer but not parse-float, just install it". "To see what's in a hash-table, I'll give you a snippet". etc.

CIEL is bloated. How can I build a lighter one?
-----------------------------------------------

First and foremost, if you think there is room for a lighter CIEL
package, come discuss in the Github issues. We can maybe create and
independent package with lighter dependencies.

Otherwise, you can have a look to this [core-dumper](https://gitlab.com/ambrevar/lisp-repl-core-dumper) tool.


What is CIEL for?
-----------------

Please see the project's homepage, and write to us if it is not clear enough!

## About CIEL scripting

### Is CIEL like Babashka for Clojure?

Babashka is a popular Clojure tool that is:

- a fast-starting scripting environment
- a standalone binary
- a collection of useful built-in libraries

So, it looks like it is.

Babashka was made possible thanks to the GraalVM Native Image, a
technical breakthrough on the JVM world. Without it, they wouldn't
have a fast-starting scripting environment. Common Lispers on the
contrary always could build standalone binaries, with Lisp sources
compiled to machine code. So these "scripting" capabilities are not a
surprise. CIEL scripting only makes it very easy to run and share
Common Lisp scripts (with batteries included).

### Does CIEL scripting replace Roswell?

Roswell does a lot more than scripting, especially it allows to easily
install various Common Lisp implementations.

It makes it easy to share programs, we just have to run
`ros install github-handle/software-name`.

However we find easier and faster to install and run a CIEL script,
since CIEL avoids compilation times, thanks to it including various
libraries out of the box.

At the time of writing, Roswell does something (or many things) more. It allows to
[build images and executables](https://github.com/roswell/roswell/wiki/Building-images-and-executables),
and it even provides a few interesting options, like options to reduce the binary size.

### What about cl-launch?

[cl-launch](https://www.cliki.net/cl-launch) is supposed to help for
scripting. Because of its bad documentation, I have difficulties
seeing what it does and how to use it. It can maybe be helpful, but it
won't give you batteries included like CIEL does.

### Is that all the scripting options available for Common Lisp?

Of course not. For one, implementations like SBCL have the `--script` and `--load` flags.

You can use a shebang line too.

See other solutions / attempts on [awesome-cl#scripting](https://github.com/CodyReichert/awesome-cl#scripting).

### But writing Lisp code on the terminal is not fun :(

I find it fun, but don't write big one-liners to feed to `--eval` ;)
You can write your CIEL scripts using your favourite editor setup.

Also, Common Lisp strings only accept double quotes, so use single quotes for the outter eval expression, and double quotes inside.

Anyways, CIEL scripting doesn't replace a good editor setup, where you
can have all the praised [image-based interactivity](https://www.youtube.com/watch?v=jBBS4FeY7XM)
a good Lisp provides.

Also, once you have your Common Lisp development environment in place,
you can build your own standalone binaries, with or without relying on
the `:ciel` library.

<!-- But in that case, you have to build a binary for every platform yourself. CIEL scripting can help here. -->


About Common Lisp
=================

What is Common Lisp good for, really?
-------------------------------------

We have a famous quote for this question:

> Please don't assume Lisp is only useful for Animation and Graphics, AI, Bioinformatics, B2B and Ecommerce, Data Mining, EDA/Semiconductor applications, Expert Systems, Finance, Intelligent Agents, Knowledge Management, Mechanical CAD, Modeling and Simulation, Natural Language, Optimization, Research, Risk Analysis, Scheduling, Telecom, and Web Authoring just because these are the only things they happened to list.
>
> Kent Pitman

Will I get hit by the Lisp curse?
---------------------------------

The only, very serious Lisp curse we know, is that once you taste Lisp, all other languages become insipid. CIEL brings you higher in the sky, at a higher risk. Sorry!
