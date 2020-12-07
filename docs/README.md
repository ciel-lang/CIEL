# CIEL

CIEL is a collection of useful libraries.

It's Common Lisp, batteries included.

Questions, doubts? See the [FAQ](FAQ.md).

## Rationale

One of our goals is to make Common Lisp useful out of the box for
mundane tasks -by today's standards.

Consequently, we ship libraries to
handle JSON and CSV, as well as others to ease string manipulation,
to have regular expressions out of the box, for threads and
jobs scheduling, for HTTP and URI handling, to create simple GUIs with
Ltk, and so on. You can of course do all this without CIEL, but
then you'd have to install the library manager (Quicklisp) first and load these libraries
into your Lisp image every time you start it. Now, you have them at
your fingertips whenever you start CIEL. Some of the libraries we bring in are for extending the language
syntax a bit. For example, we include the `Trivia` library for
pattern matching.

We also aim to soften the irritating parts of standard Common Lisp.
A famous one, puzzling for beginners and non-optimal for seasoned
lispers, is the creation of hash-tables. We include the `dict` function
from the Serapeum library (which we enhanced further with a pull request):


~~~lisp
CIEL-USER> (dict :a 1 :b 2 :c 3)
~~~

which prints:

~~~lisp
(dict
 :A 1
 :B 2
 :C 3
)
~~~

In standard Common Lisp, the equivalent is more convoluted:

~~~lisp
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    ht)
;; #<HASH-TABLE :TEST EQUAL :COUNT 3 {1006CE5613}>
;; (and we don't get a readable representation, so our example is not even equivalent)
~~~

We also add some missing functions. For example, after you used `parse-integer`, you are probably looking for a `parse-float` function… but you can't find it, because it isn't defined by the standard. You must install a third-party library. Now you have it.

Moreover, we want to bring a **user friendly REPL on the terminal**,
with bells and whistles useful to the developer and people living in a
terminal window. For example, our [repl for the terminal](repl.md) has readline support, multi-line editing, optional syntax highlighting, a shell passthrough, and more goodies.


# Install

Let's get started. See the [installation instructions](install.md).


# Final words

That was your life in CL:

<p align="center"><img src="before.jpeg" /></p>
and now:

<p align="center"><img src="after-plus.jpeg" /></p>
