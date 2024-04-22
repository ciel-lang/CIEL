# CIEL

CIEL is a ready-to-use collection of Lisp libraries.

It's Common Lisp, batteries included.

It comes in 3 forms:

- a binary, to run CIEL **scripts**: fast start-up times, standalone binary, built-in utilities.
- a simple full-featured **REPL** for the terminal.
- a **Lisp library**.

Questions, doubts? See the [FAQ](FAQ.md).

Status: it's a work in progress. I deployed it for client projects.

```lisp
#!/usr/bin/env ciel

(-> "https://fakestoreapi.com/products?limit=5"
  http:get
  json:read-json
  (elt 0)
  (access "title"))
```

```bash
$ chmodx +x getproduct.lisp
$ time ./getproduct.lisp
"Fjallraven - Foldsack No…ckpack, Fits 15 Laptops"
./getproduct.lisp  0.10s user 0.02s system 24% cpu 0.466 total
```



## Rationale

One of our goals is to make Common Lisp useful out of the box for
mundane tasks -by today's standards.

Consequently, **we ship libraries** to
handle JSON and CSV, as well as others to ease string manipulation,
to have regular expressions out of the box, for threads and
jobs scheduling, for HTTP and URI handling, to create simple GUIs with
nodgui (Tk-based, nice theme), and so on. You can of course do all this without CIEL, but
then you'd have to install the library manager (Quicklisp) first and load these libraries
into your Lisp image every time you start it. Now, you have them at
your fingertips whenever you start CIEL. Some of the libraries we bring in are for extending the language
syntax a bit. For example, we include the `Trivia` library for
pattern matching.

We also aim to **soften the irritating parts of standard Common Lisp**.
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

We **add missing functions**. For example, after you used `parse-integer`, you are probably looking for a `parse-float` function… but you can't find it, because it isn't defined by the standard. You must install a third-party library. Now you have it.

We **enhance the docstrings** of built-in functions and macros with more
explanations and examples, so you don't have to reach to external
documentation just yet in order to understand and try out things like
`mapcar` or `loop` (look 'ma, LOOP has no docstring by default).

Moreover, we bring a **user friendly REPL on the terminal**,
with bells and whistles useful to the developer and people living in a
terminal window. For example, our [repl for the terminal](repl.md) has readline support, multi-line editing, optional syntax highlighting, a shell passthrough, and more goodies.

We bring **scripting capabilities**. Just run `ciel myscript.lisp`, and use all the high-level CIEL libraries and Common Lisp features in your script. It starts up fast.


*We are only started. You can sponsor us [on GitHub sponsors](https://github.com/sponsors/vindarel/), thank you!*

[![ko-fi](https://www.ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/K3K828W0V)

*If you need to learn Common Lisp the efficient way, I have [a course on videos](https://www.udemy.com/course/common-lisp-programming/?referralCode=2F3D698BBC4326F94358), with many real-world practical stuff in and still growing. If you are a student, drop me a line for a coupon. Thank you for your support!*


# Install

Let's get started. See the [installation instructions](install.md).


# Final words

That was your life in CL:

<p align="center"><img src="before.jpeg" /></p>
and now:

<p align="center"><img src="after-plus.jpeg" /></p>
