
## Bind, more destructuring in `let` (metabang-bind)

We import the `bind` macro from [metabang-bind](https://common-lisp.net/project/metabang-bind/user-guide.html) ([GitHub](https://github.com/gwkkwg/metabang-bind)).

The idiomatic way to declare local variables is `let`. Use it if it is fine for you.

However, if you ever noticed to write convoluted `let` forms, adding
list destructuring, multiple values or slot access into the mix, then
read on.

`bind` integrates more variable binding and list destructuring idioms. It has two goals. Quoting:

> 1. reduce the number of nesting levels

> 2. make it easier to understand all of the different forms of destructuring and variable binding by unifying the multiple forms of syntax and reducing special cases.

### Bind in CIEL

We import the `bind` macro. However, the package has more external
symbols that we don't import, such as its error type (`bind-erro`) and
its extension mechanism.


### Bind is a replacement for `let` and `let*`.

You can use `bind` in lieue of `let*`.

So, its simpler form is:

~~~lisp
(bind (a)
  (do-something a))
~~~

`a` is initialized to `nil`.

To give it a default value, use a pair, as in `(a 1)` below:

~~~lisp
(bind ((a 1)
       (b 2))
  ...)
~~~

Below, we'll use indicators for `a`, the binding on the left-hand
side, so we can have a `bind` form that starts with three
parenthesis. But it's OK, you know how to read them.

~~~lisp
(bind (((:values a b c) (a-function)))
  ...)
~~~


### Bind with multiple-values: `(:values ...)`

Use `(:values ...)` in the left-hand side of the binding:

~~~lisp
(bind (((:values a b) (truncate 4.5))
       ...
~~~

In pure CL, you'd use `multiple-value-bind` (aka mvb for completion).


### Ignore values: the `_` placeholder

As in:

~~~lisp
(bind (((_ value-1 value-2) (some-function-returning-3-values)))
       ...)
~~~

### Destructuring lists

Use a list in the left-hand side:

~~~lisp
(defun return-list (a b) (list a b))

(bind (((a b) (return-list 3 4)))
  (list a b))
;; => (3 4)
~~~

You can use usual lambda parameters for more destructuring:

~~~lisp
(bind ((a 2)
       ((b &rest args &key (c 2) &allow-other-keys) '(:a :c 5 :d 10 :e 54))
       ((:values d e) (truncate 4.5)))
  (list a b c d e args))
~~~

### Bind with plists, arrays, classes, structures, regular expressions, flet and labels

It's all well explained [in the documentation](https://common-lisp.net/project/metabang-bind/user-guide.html)!

### See also: Trivia pattern matching

If you like object destructuring in general, you'll like pattern matching with [Trivia](https://github.com/guicho271828/trivia/) (also available in CIEL).
