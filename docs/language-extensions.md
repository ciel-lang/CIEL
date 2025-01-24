## Data structures

### Generic and nested access to datastructures (access)

From [Access](https://github.com/AccelerationNet/access/), we import `access` and `accesses` (plural).

It's always

```lisp
(access my-structure :elt)
```

for an alist, a hash-table, a struct, an object… Use `accesses` for nested access (specially useful with JSON). See also `json-pointer`.

### Hash-table utilities (Alexandria and Serapeum)

We import functions from [Alexandria](https://alexandria.common-lisp.dev/draft/alexandria.html#Hash-Tables) and [Serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#hash-tables).

To see their full list with their documentation, see [alexandria](alexandria.md) [serapeum](serapeum.md).

```txt
;; alexandria
hash-table-keys
hash-table-values
ensure-gethash
```

``` txt
;; serapeum
:dict
:do-hash-table ;; see also trivial-do
:dict*
:dictq  ;; quoted
:href  ;; for nested lookup.
:href-default
:pophash
:swaphash
:hash-fold
:maphash-return
:merge-tables
:flip-hash-table
:set-hash-table
:hash-table-set
:hash-table-predicate
:hash-table-function
:make-hash-table-function
:delete-from-hash-table
:pairhash
```

Here's how we can create a hash-table with keys and values:

``` lisp
;; create a hash-table:
(dict :a 1 :b 2 :c 3)
;; =>
(dict
 :A 1
 :B 2
 :C 3
)
```

In default Common Lisp, you would do:

```lisp
  (let ((ht (make-hash-table :test 'equal)))
    (setf (gethash :a ht) 1)
    (setf (gethash :b ht) 2)
    (setf (gethash :c ht) 3)
    ht)
;; #<HASH-TABLE :TEST EQUAL :COUNT 3 {1006CE5613}>
```

As seen above, hash-tables are pretty-printed by default.

You can toggle the representation with `toggle-pretty-print-hash-table`, or by setting

```lisp
(setf *pretty-print-hash-tables* nil)
```

in your configuration file.

### Sequences utilities (Alexandria, Serapeum)

From [Serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#sequences) we import:

``` txt
:assort
:batches
:filter
:runs
:partition
:partitions
:split-sequence
```

And from [Alexandria](https://common-lisp.net/project/alexandria/draft/alexandria.html):

``` text
:iota
:flatten
:shuffle
:random-elt
:length=
:last-elt
:emptyp
```

From `alexandria-2` we import:

```text
:subseq*  (the end argument can be larger than the sequence's length)
```

and some more.

### String manipulation (str)

Available with the `str` prefix.

It provides functions such as: `trim`, `join`, `concat`, `split`, `repeat`, `pad`, `substring`, `replace-all`, `emptyp`, `blankp`, `alphanump`, `upcase`, `upcasep`, `remove-punctuation`, `from-file`, `to-file`…

See <https://github.com/vindarel/cl-str/> and https://lispcookbook.github.io/cl-cookbook/strings.html

## Arrow macros

We provide the Clojure-like arrow macros and "diamond wands" from the [arrow-macros](https://github.com/hipeta/arrow-macros) library.
<!-- tabs:start -->

#### **CIEL**

```lisp
;; -> inserts the previous value as its first argument:
(-> "  hello macros   "
  str:upcase
  str:words) ; => ("HELLO" "MACROS")

;; ->> inserts it as its second argument:
(->> "  hello macros   "
  str:upcase
  str:words
  (mapcar #'length)) ; => (5 6)


;; use as-> to be flexible on the position of the argument:
(as-> 4 x
  (1+ x)
  (+ x x)) ; => 10
```

#### **CL**

```lisp
;; In pure CL, just wrap function calls…
(mapcar #'length (str:words (str:upcase "  hello world ")))

;; … or use let* and intermediate variables:
(let* ((var "hello macros")
       (upcased (str:upcase var))
       (words (str:words upcased)))
  words)
```

<!-- tabs:end -->

And there is more. All the available macros are:

``` txt
:->
:->>
:some->
:some->>
:as->
:cond->
:cond->>
:-<>
:-<>>
:some-<>
:some-<>>
```

## Functions

### Partial application

We import Serapeum's `partial` and Alexandria's `rcurry`. They allow
partial application of functions.

`partial` is similar to `alexandria:curry` but is always inlined.

We import Serapeum's `juxt`.

You can check [Serapeum's function helpers](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#functions),
the non-imported ones are only a `serapeum` package prefix away in CIEL if you need
them. Other noticeable functions: `distinct`, `once`, `throttle`, `trampoline`, `do-nothing`…

### Memoization

We import `defcached` from
[function-cache](https://github.com/AccelerationNet/function-cache), a
library that provides extensible caching/memoization.

For example:

```lisp
(defcached (foo :timeout 10) (arg)
  (sleep 3)
  arg)
```

Run `(foo 1)`, it sleeps 3 seconds and returns 1. Run `(foo 1)` again
and it returns 1 immediately. Its result (for this argument) is cached
for 10 seconds.



## Bind, more destructuring in `let` (metabang-bind)

We import the `bind` macro from [metabang-bind](https://common-lisp.net/project/metabang-bind/user-guide.html) ([GitHub](https://github.com/gwkkwg/metabang-bind)).

The idiomatic way to declare local variables is `let` (and `let*`),
the way to declare local functions is `flet` (and `labels`). Use them
if it is fine for you.

However, if you ever noticed you write convoluted `let` forms, adding
list destructuring, multiple values or slot access into the mix, and
if you use a `flet` *and then* a `let`, then read on.

`bind` integrates more variable binding and list destructuring idioms. It has two goals. Quoting:

> 1. reduce the number of nesting levels

> 2. make it easier to understand all of the different forms of destructuring and variable binding by unifying the multiple forms of syntax and reducing special cases.

### Bind in CIEL

We import the `bind` macro. However, the package has more external
symbols that we don't import, such as its error type (`bind-error`) and
its extension mechanism.

> Note: if you like object destructuring in general, you'll like [pattern matching](/language-extensions?id=pattern-matching).


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


Conditions
----------

See <https://lispcookbook.github.io/cl-cookbook/error_handling.html>

From Serapeum, we import [`ignoring`](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#ignoring-type-body-body).

An improved version of `ignore-errors`. The behavior is the same: if an error occurs in the body, the form returns two values, nil and the condition itself.

`ignoring` forces you to specify the kind of error you want to ignore:

```lisp
(ignoring parse-error
          ...)
```

## Iteration

See <https://lispcookbook.github.io/cl-cookbook/iteration.html> for examples, including about the good old `loop`.

We import macros from [trivial-do](https://github.com/yitzchak/trivial-do/), that provides `dolist`-like macro to iterate over more structures:

- `dohash`: dohash iterates over the elements of an hash table and binds key-var to the key, value-var to the associated value and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

- `doplist`: doplist iterates over the elements of an plist and binds key-var to the key, value-var to the associated value and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

- `doalist`: doalist iterates over the elements of an alist and binds key-var to the car of each element, value-var to the cdr of each element and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

- `doseq*`: doseq\* iterates over the elements of an sequence and binds position-var to the index of each element, value-var to each element and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

- `doseq`: doseq iterates over the elements of an sequence and binds value-var to successive values and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

- `dolist*`: dolist\* iterates over the elements of an list and binds position-var to the index of each element, value-var to each element and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

We ship [`for`](https://github.com/Shinmera/for) so you can try it, but we don't import its symbols. `for`'s `over` keyword allows to loop across all data structures (lists, hash-tables…).


## Lambda shortcut

`^` is a synonym macro for `lambda`.

```lisp
(^ (x) (+ x 10))
=>
(lambda (x) (+ x 10))
```

## Pythonic triple quotes docstring

We can enable the syntax to use triple quotes for docstrings, and double quotes within them:

<!-- tabs:start -->

#### **CIEL**
```lisp
(ciel:enable-pythonic-string-syntax)

(defun foo ()
  """foo "bar"."""
  t)
```

#### **CL**

~~~lisp
;; Normally single quotes must be escaped.
(defun foo ()
   "foo \"bar\"."
   t)

;; use:
(pythonic-string-reader:enable-pythonic-string-syntax)
~~~
<!-- tabs:end -->

To disable this syntax, do:

~~~lisp
(ciel:disable-pythonic-string-syntax)
~~~

We use [pythonic-string-reader](https://github.com/smithzvk/pythonic-string-reader).

!> This syntax conflicts with libraries that use cl-syntax to use triple quotes
too, even only internally. It happens with the Jonathan library.


## Packages

`defpackage` is nice and well, until you notice some shortcomings. That's why we import UIOP's `define-package`. You'll get:

- less warnings when you remove an exported symbol
- a `:reexport` option (as well as `:use-reexport` and `:mix-reeport`)
- `:recycle` and `:mix` options.

It is a drop-in replacement.

Here's [uiop:define-package documentation](https://asdf.common-lisp.dev/uiop.html#UIOP_002fPACKAGE).

### Packages local nicknames

CIEL defines local nicknames for other libraries.

For example, `csv` is a shorter nickname for `cl-csv`. `time` is a
shorter nickname for `local-time`.

They are available when you are "inside" the CIEL-USER package (when you do `(in-package :ciel-user)`).

If you define a new package that "uses" CIEL, you might want to also
get this set of nicknames. Here's the full list:

~~~lisp
(uiop:define-package myproject
    (:use :cl :ciel)
    (:local-nicknames (:/os :uiop/os)
                      (:os :uiop/os)
                      (:filesystem :uiop/filesystem)
                      (:finder :file-finder)
                      (:notify :org.shirakumo.file-notify)
                      (:alex :alexandria)
                      (:csv :cl-csv)
                      (:http :dexador)
                      (:json :shasht)
                      (:json-pointer :cl-json-pointer/synonyms)
                      (:time :local-time)
                      (:routes :easy-routes))
    (:documentation "My package, using CIEL and defining the same local nicknames."))
~~~



Pattern matching
----------------

We use [Trivia](https://github.com/guicho271828/trivia/) (see
[its wiki](https://github.com/guicho271828/trivia/wiki/What-is-pattern-matching%3F-Benefits%3F)), from which we import `match`.

You can start typing "match", followed by the object to match against, and the clauses, which are similar to a `cond`. Here's an example to match a list:

~~~lisp
(match '(1 2)
  ((list x y)  ;; <-- pattern
   (print x)
   (print y))
  (_           ;; <-- failover clause
    :else))
;; 1
;; 2
~~~

On the above example, `(list x y)` is the pattern. It binds `x` to 1 and `y` to 2. Pay attention that the `list` pattern is "strict": it has two subpatterns (x and y) and it will thus match against an object of length 2.

If you wanted `y` to match the rest of the list, use `list*`:

~~~lisp
(match '(1 2 3)
  ((list* x y)
   (print x)
   (print y))
  (_ :else))
;; 1
;; (2 3)
~~~

This could also be achieved with the `cons` pattern:

~~~lisp
(match '(1 2 3)
   ((cons x y)
    (print x)
    (print y))
   (_ :else))
;; 1
;; (2 3)
~~~

You can of course use `_` placeholders:

~~~lisp
(match '(1 2 3)
  ((list* x _)
   (print x))
  (_ :else))
;; 1
~~~

As we saw with `list` and `cons`, Trivia has patterns to match against types (vectors, alists, plists, arrays), including classes and structures.

You can use [numeric patterns](https://github.com/guicho271828/trivia/wiki/Numeric-Patterns) (`=`, `<` and friends, that behave as you expect):

~~~lisp
(let ((x 5))
   (match x
     ((< 10)
      :lower)))
;; :LOWER
~~~

Then, you can combine them with [logic based patterns and guards](https://github.com/guicho271828/trivia/wiki/Logic-Based-Patterns). For example:

~~~lisp
(match x
  ((or (list 1 a)
       (cons a 3))
   a))
~~~

guards allow to check the matches against a predicate. For example:

~~~lisp
(match (list 2 5)
  ((guard (list x y)     ; subpattern1
          (= 10 (* x y))) ; test-form
   t))
~~~

Above we use the `list` pattern, and we verify a predicate.

Trivia has more tricks in its sleeve. See the [special patterns](https://github.com/guicho271828/trivia/wiki/Special-Patterns) (access and change objects), the [ppcre contrib](https://github.com/guicho271828/trivia/wiki/Contrib-packages), etc.

You migth also be interested in exhaustiveness type checking explained just below.


## Regular expressions

Use `ppcre`.

See <https://common-lisp-libraries.readthedocs.io/cl-ppcre> and <https://lispcookbook.github.io/cl-cookbook/regexp.html>


Type declarations
-----------------

Use the `-->` macro to gradually add type declarations.

Alternatively, use `defun*`, `defgeneric*`, `defmethod*`, `defparameter*` and `defvar*` to add type declarations directly in the lambda list.

These notations are not strictly equivalent though.

`-->` comes from [Serapeum](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#types). It is a shortcut for `(declaim (ftype my-function (… input types …) … return type …))`

<!-- tabs:start -->

#### **CIEL**

```lisp
(--> mod-fixnum+ (fixnum fixnum) fixnum)
(defun mod-fixnum+ (x y) ...)

;; --> comes straight from serapeum:->
```

#### **CL**

```lisp
(declaim (ftype (function (fixnum fixnum) fixnum) mod-fixnum+))
(defun mod-fixnum+ (x y) ...)
```
<!-- tabs:end -->

Now `defun*` and friends allow to add type declarations directly in the lambda list. They add the `(declaim (ftype` as above, and additionnaly `declare` types inside the function body:

<!-- tabs:start -->

#### **CIEL**

```lisp
(defun* foo ((a integer))
  (:returns integer)
  (* 10 a))
```

#### **CL**

```lisp
;; In pure CL, type the functions at its boundaries with ftype.
;; It is a bit verbose, but it has the advantage, being not tied to defun,
;; that we can easily refine types during development.
(declaim (ftype (function (integer) integer)             foo))
;;                        ^^ inputs ^^ output [optional] ^^ function name

;; defstar adds the internal "declare" and "the…".
;; "the" is a promise made to the compiler, that will optimize things out.
(defun foo (a)
  (declare (type integer a))
  (the integer (* 10 a)))

```
<!-- tabs:end -->

A type declaration for a parameter:

<!-- tabs:start -->

#### **CIEL**

```lisp
(defparameter* (*file-position* (integer 0)) 0)
```

#### **CL**

```lisp

;; Normal defparameter:
(defparameter *file-position* 0)

;; Assigning a bad value works:
(setf *file-position* "8")
;; "8"

;; We add a type declaration:
(declaim (type (integer 0) *file-position*))

;; and now:
(setf *file-position* "8")
;;
;; Value of #1="8" in (THE INTEGER "8") is #1#, not a INTEGER.
;;   [Condition of type SIMPLE-TYPE-ERROR]
;;
;; we get a type error.
```
<!-- tabs:end -->

We can use any type specifier:

~~~lisp
(deftype natural () '(real 0))
(defun* sum  ((a natural) (b natural))
  (:returns natural)
  (+ a b))
~~~

Now, we get type errors at compile time:

~~~lisp
(foo "3")
;; =>
The value
  "3"
is not of type
  INTEGER
when binding A
   [Condition of type TYPE-ERROR]

Restarts: […]

Backtrace:
  0: (FOO "3") [external]
  1: (SB-INT:SIMPLE-EVAL-IN-LEXENV (FOO "a") #<NULL-LEXENV>)
  2: (EVAL (FOO "3"))
~~~

and we get compile-time warnings on type mismatches (but to be honest on simple cases like this SBCL is already quite good):

~~~lisp
(defun* bad-foo ((a integer))
  (:returns integer)
  (format t "~a" (* 10 a)))
;
; in: DEFUN* BAD-FOO
;     (THE INTEGER (FORMAT T "~a" (* 10 CIEL::A)))
;
; caught WARNING:
;   Constant NIL conflicts with its asserted type INTEGER.
;   See also:
;     The SBCL Manual, Node "Handling of Types"
;
; compilation unit finished
;   caught 1 WARNING condition
BAD-FOO
~~~

We could add extra protection and a `check-type`, evaluated at runtime.
Defstar can add them automatically if `defstar:*check-argument-types-explicitly?*` is non-nil.

In theory, such declarations don't guarantee that Lisp will do type checking but in practice the implementations, and in particular SBCL, perform type checking.

We use the [defstar](https://github.com/lisp-maintainers/defstar) library. Its README has many more examples and even more features (adding assertions, `:pre` and `:post` clauses).

> Note: we are not talking thorough ML-like type checking here (maybe the [Coalton](https://github.com/stylewarning/coalton) library will bring it to Common Lisp). But in practice, the compiler warnings and errors are helpful during development, "good enough", and SBCL keeps improving in that regard.

> Note: there is no "undeclaim" form :] You can unintern a symbol and re-define it.

See also:

- [declarations](http://clhs.lisp.se/Body/03_c.htm) in the Common Lisp Hyper Spec.
- https://lispcookbook.github.io/cl-cookbook/type.html
- the article [Static type checking in SBCL](https://medium.com/@MartinCracauer/static-type-checking-in-the-programmable-programming-language-lisp-79bb79eb068a), by Martin Cracauer
- the article [Typed List, a Primer](https://alhassy.github.io/TypedLisp) - let's explore Lisp's fine-grained type hierarchy! with a shallow comparison to Haskell.


Type checking: exhaustiveness type checking
-------------------------------------------

Write a "case" and get a compile-time warning if you don't cover all cases.

From Serapeum, we import:

```lisp
:etypecase-of
:ctypecase-of
:typecase-of
:case-of
:ccase-of
```

`etypecase-of` allows to do [compile-time exhaustiveness type checking](https://github.com/ruricolist/serapeum#compile-time-exhaustiveness-checking%0A).

### Example with enums

We may call a type defined using member an enumeration. Take an enumeration like this:

```lisp
(deftype switch-state ()
  '(member :on :off :stuck :broken))
```

Now we can use `ecase-of` to take all the states of the switch into account.

```lisp
(defun flick (switch)
  (ecase-of switch-state (state switch)
    (:on (switch-off switch))
    (:off (switch-on switch))))
=> Warning
```

```lisp
(defun flick (switch)
  (ecase-of switch-state (state switch)
    (:on (switch-off switch))
    (:off (switch-on switch))
    ((:stuck :broken) (error "Sorry, can't flick ~a" switch))))
=> No warning
```

### Example with union types

```lisp
(defun negative-integer? (n)
  (etypecase-of t n
    ((not integer) nil)
    ((integer * -1) t)
    ((integer 1 *) nil)))
=> Warning

(defun negative-integer? (n)
  (etypecase-of t n
    ((not integer) nil)
    ((integer * -1) t)
    ((integer 1 *) nil)
    ((integer 0) nil)))
=> No warning
```

See [Serapeum's reference](https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#control-flow).

## trivial-types: more type definitions

From [trivial-types](https://github.com/m2ym/trivial-types), we import

-   `association-list-p`
-   `type-expand`
-   `string-designator`
-   `property-list`
-   `tuple`
-   `association-list`
-   `character-designator`
-   `property-list-p`
-   `file-associated-stream-p`
-   `type-specifier-p`
-   `list-designator`
-   `package-designator`
-   `tuplep`
-   `non-nil`
-   `file-associated-stream`
-   `stream-designator`
-   `function-designator`
-   `file-position-designator`
-   `pathname-designator`
