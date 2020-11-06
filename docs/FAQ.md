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

What is CIEL for?
-----------------

Please see the project's homepage, and write to us if it is not clear enough!

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
