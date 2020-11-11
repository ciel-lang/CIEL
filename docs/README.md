# CIEL

CIEL is a collection of useful libraries.

It's Common Lisp, batteries included.

Questions, doubts? See the [FAQ](FAQ.md).

# Install

## With Quicklisp

You need a Lisp implementation and Quicklisp installed.

CIEL is not yet on Quicklisp (but it is on [Ultralisp](https://ultralisp.org)), so clone this repository and load the .asd (with `load` or `C-c C-k` in Slime).

``` example
git clone https://github.com/ciel-lang/CIEL ~/quicklisp/local-projects/CIEL
```

Then, quickload it:

```lisp
(ql:quickload "ciel")
```

and enter the `ciel-user` package, instead of the default `common-lisp-user` (or `cl-user`):

```lisp
(in-package :ciel-user)
```

## With a core image

You need a Lisp implementation, but you don't need Quicklisp.

Build a *core image* for your lisp with all CIEL's dependencies:

``` example
sbcl --load build-image.lisp
```

and use it:

``` example
sbcl --core ciel --eval '(in-package :ciel-user)'
```

TODO: we will distribute ready-to-use core images.

## With a binary

You don't need anything, just download the CIEL executable and run its REPL.

TODO: build it on CI for different platforms.

To build it, clone this repository and run `make build`.

Start it with `./ciel-repl`.

You are dropped into a custom Lisp REPL.

# CIEL's custom REPL

This REPL is more user friendly than the default SBCL one:

-  it has readline capabilities, meaning that the arrow keys work by default (wouhou!) and there is a persistent history, like in any shell.
-  it has **multiline input**.
-  it has **TAB completion**.
-  it handles errors gracefully: you are not dropped into the debugger and its sub-REPL, you simply see the error message.
-  it has optional **syntax highlighting**.

-  it defines short **helper commands**:

``` txt
:help => Prints this general help message
:doc => Prints the available documentation for this symbol
:? => Gets help on a symbol <sym>: :? str
:w => Writes the current session to a file <filename>
:d => Dumps the disassembly of a symbol <sym>
:t => Prints the type of a expression <expr>
:lisp-critic => Toggles the lisp-critic
:q => Ends the session.
```

- it has a **shell pass-through**: try `!ls`.

Our REPL is adapted from [sbcli](https://github.com/hellerve/sbcli). See also [cl-repl](https://github.com/koji-kojiro/cl-repl/), that has an interactive debugger.

## Quick documentation lookup

The documentation fo a symbol is available with `:doc` and also by
appending a "?" after a function name:

```
ciel-user> :doc dict
;; or:
ciel-user> (dict ?
```

# Shell pass-through

Use `!` to send a shell command:

```
!ls
Makefile
README.org
repl.lisp
repl-utils.lisp
src
...

!pwd
/home/vindarel/projets/ciel
```

Use square brackets `[...]` to write a shell script, and use `$` inside it to escape to lisp:

```lisp
(dotimes (i 7) (princ [echo ?i]))
```

The result is concatenated into a string and printed on stdout.

This feature is only available in CIEL's REPL, not on the CIEL-USER package.

We use the [Clesh](https://github.com/Neronus/clesh) library.

See also [SHCL](https://github.com/bradleyjensen/shcl) for a more unholy union of posix-shell and Common Lisp.


## Syntax highlighting

Syntax highlighting is off by default. To enable it, install [pygments](https://pygments.org/) and add this in your `~/.cielrc`:

```lisp
(in-package :sbcli)
(setf *syntax-highlighting* t)

;; and, optionally:
;; (setf *pygmentize* "/path/to/pygmentize")
;; (setf *pygmentize-options* (list "-s" "-l" "lisp"))
```

You can also switch it on and off from the REPL:

```lisp
(setf sbcli:*syntax-highlighting* t)
```

## Friendly lisp-critic

The `:lisp-critic` helper command toggles on and off the
[lisp-critic](https://github.com/g000001/lisp-critic). The Lisp Critic
scans your code for instances of bad Lisp programming practice. For
example, when it sees the following function:


~~~lisp
(critique
   (defun count-a (lst)
     (setq n 0)
     (dolist (x lst)
       (if (equal x 'a)
         (setq n (+ n 1))))
     n))
~~~

the lisp-critic gives you these advices:

```
----------------------------------------------------------------------

SETS-GLOBALS: GLOBALS!! Don't use global variables, i.e., N
----------------------------------------------------------------------

DOLIST-SETF: Don't use SETQ inside DOLIST to accumulate values for N.
Use DO. Make N a DO variable and don't use SETQ etc at all.
----------------------------------------------------------------------

USE-EQL: Unless something special is going on, use EQL, not EQUAL.
----------------------------------------------------------------------

X-PLUS-1: Don't use (+ N 1), use (1+ N) for its value or (INCF N) to
change N, whichever is appropriate here.
----------------------------------------------------------------------
; in: DEFUN COUNT-A
;     (SETQ CIEL-USER::N 0)
;
; caught WARNING:
;   undefined variable: N
;
; compilation unit finished
;   Undefined variable:
;     N
;   caught 1 WARNING condition
=> COUNT-A
```


# Libraries

To see the full list of dependencies, see the `ciel.asd` project definition or this [dependencies list](dependencies.md).

## Data structures

### Generic and nested access to datastructures (access)

From [Access](https://github.com/AccelerationNet/access/%0A), we import `access` and `accesses` (plural).

It's always

```lisp
(access my-structure :elt)
```

for an alist, a hash-table, a struct, an object… Use `accesses` for nested access (specially useful with JSON).

### Hash-table utilities (Serapeum)

We import functions from Serapeum. <https://github.com/ruricolist/serapeum/blob/master/REFERENCE.md#hash-tables>

To see their full list with their documentation, see [serapeum](serapeum.md).
``` txt
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

You can toggle the representation with `toggle-print-hash-table`, or by setting

```lisp
(setf *pretty-print-hash-tables* nil)
```

in your configuration file.

### Sequences utilities (Alexandria, Serapeum)

From *Serapeum* we import:

``` txt
:assort
:batches
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

and some more.

### String manipulation (str)

Available with the `str` prefix.

<https://github.com/vindarel/cl-str/>

Data formats
------------

### CSV

You have [cl-csv](https://github.com/AccelerationNet/cl-csv), under its `cl-csv` package name and the `csv` local nickname.

```lisp
;; read a file into a list of lists
(cl-csv:read-csv #P"file.csv")
=> (("1" "2" "3") ("4" "5" "6"))

;; read csv from a string (streams also supported)
(cl-csv:read-csv "1,2,3
4,5,6")
=> (("1" "2" "3") ("4" "5" "6"))

;; read a file that's tab delimited
(cl-csv:read-csv #P"file.tab" :separator #\Tab)

;; loop over a CSV for effect
(let ((sum 0))
  (cl-csv:do-csv (row #P"file.csv")
    (incf sum (parse-integer (nth 0 row))))
  sum)
```

See also:

-   [auto-text](https://github.com/defunkydrummer/auto-text), automatic detection for text files (encoding, end of line, column width, csv delimiter etc). [inquisitor](https://github.com/t-sin/inquisitor) for detection of asian and far eastern languages.
-   [CLAWK](https://github.com/sharplispers/clawk), an AWK implementation embedded into Common Lisp, to parse files line-by-line.

### JSON

We use [cl-json](https://common-lisp.net/project/cl-json/cl-json.html) ([GitHub](https://github.com/hankhero/cl-json)). It has a `json` nickname.

To encode an object to a string, use `encode-json-to-string`:

```lisp
(json:encode-json-to-string (list (dict :a 1)))
;; "[{\"A\":1}]"
```

To decode from a string: `decode-json-from-string`.

To encode or decode objects from a *stream*, use:

-   `encode-json object &optional stream`
-   `decode-json &optional stream`

as in:

```lisp
(with-output-to-string (s)
   (json:encode-json (dict :foo (list 1 2 3)) s))
;; "{\"FOO\":[1,2,3]}"

(with-input-from-string (s "{\"foo\": [1, 2, 3], \"bar\": true, \"baz\": \"!\"}")
  (json:decode-json s))
;; ((:|foo| 1 2 3) (:|bar| . T) (:|baz| . "!"))
```

cl-json can encode and decode from objects. Given a simple class:

```lisp
(defclass person ()
  ((name :initarg :name)
   (lisper :initform t)))
```

We can encode an instance of it:

```lisp
(json:encode-json-to-string (make-instance 'person :name "you"))
;; "{\"NAME\":\"you\",\"LISPER\":true}"
```

By default, cl-json wants to convert our lisp symbols to camelCase, and the JSON ones to lisp-case. We disable that in the `ciel-user` package.

You can set this behaviour back with:

```lisp
(setf json:*json-identifier-name-to-lisp* #'json:camel-case-to-lisp)
(setf json:*lisp-identifier-name-to-json* #'json:lisp-to-camel-case)
```

Date and time
-------------

The [local-time](https://common-lisp.net/project/local-time/) package is available.

See also [awesome-cl\#date-and-time](https://github.com/CodyReichert/awesome-cl#date-and-time) and the [Cookbook](https://lispcookbook.github.io/cl-cookbook/dates_and_times.html).

Databases
---------

Mito and SxQL are available.

<https://lispcookbook.github.io/cl-cookbook/databases.html>

GUI (ltk)
---------

We ship [ltk](http://www.peter-herth.de/ltk/ltkdoc/).

The Tk toolkit is nearly ubiquitous and simple to use. It doesn't have a great deal of widgets, but it helps anyways for utility GUIs. Moreover, it doesn't look aweful (as it did back), it has themes to look nearly native on the different platforms.

Here's how it looks like on Mac:

![](https://lispcookbook.github.io/cl-cookbook/assets/gui/ltk-on-macos.png)

You have other GUI options a quickload away (Qt4, Gtk, IUP, Nuklear, not mentioning LispWorks CAPI…): <https://lispcookbook.github.io/cl-cookbook/gui.html>

Here's how to start with Ltk:

-   either put yourself in the `ltk-user` package:

```lisp
(in-package :ltk-user)
```

-   either `use` ltk:

```lisp
(use-package :ltk)
```

Use the `with-ltk` macro to define your GUI, use `make-instance` + a widget name to create it, and use the `grid` to position widgets.

```lisp
(with-ltk ()
  (let ((button (make-instance 'button :text "hello")))
    (grid button 0 0)))
```

Read more: <https://lispcookbook.github.io/cl-cookbook/gui.html#tk>

Iteration
---------

We ship `iterate` and `for` so you can try them, but we don't import their symbols.

See <https://lispcookbook.github.io/cl-cookbook/iteration.html> for examples, including about the good old `loop`.

We import macros from [trivial-do](https://github.com/yitzchak/trivial-do/), that provides `dolist`-like macro to iterate over more structures:

-   `dohash`: dohash iterates over the elements of an hash table and binds key-var to the key,

value-var to the associated value and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

-   `doplist`: doplist iterates over the elements of an plist and binds key-var to the key, value-var to

the associated value and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

-   `doalist`: doalist iterates over the elements of an alist and binds key-var to the car of each element,

value-var to the cdr of each element and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

-   `doseq*`: doseq\* iterates over the elements of an sequence and binds position-var to the index of each

element, value-var to each element and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

-   `doseq`: doseq iterates over the elements of an sequence and binds value-var to successive values

and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

-   `dolist*`: dolist\* iterates over the elements of an list and binds position-var to the index of each

element, value-var to each element and then evaluates body as a tagbody that can include declarations. Finally the result-form is returned after the iteration completes.

Pattern matching
----------------

Use Trivia, also available with the `match` local nickname.

Numerical and scientific
------------------------

We import `mean`, `variance`, `median` and `clamp` from Alexandria.

We import functions to parse numbers (Common Lisp only has `parse-integer` by default).

[parse-float](https://github.com/soemraws/parse-float)

Similar to PARSE-INTEGER, but parses a floating point value and returns the value as the specified TYPE (by default `*READ-DEFAULT-FLOAT-FORMAT*`). The DECIMAL-CHARACTER (by default \#.) specifies the separator between the integer and decimal parts, and the EXPONENT-CHARACTER (by default \#e, case insensitive) specifies the character before the exponent. Note that the exponent is only parsed if RADIX is 10.

``` text
ARGLIST: (string &key (start 0) (end (length string)) (radix 10) (junk-allowed nil)
        (decimal-character .) (exponent-character e)
        (type *read-default-float-format*))
```

From [parse-number](https://github.com/sharplispers/parse-number), we import:

``` text
:parse-number
:parse-positive-real-number
:parse-real-number
```

``` text
PARSE-NUMBER
  FUNCTION: Given a string, and start, end, and radix parameters,
  produce a number according to the syntax definitions in the Common
  Lisp Hyperspec.
  ARGLIST: (string &key (start 0) (end nil) (radix 10)
          ((float-format *read-default-float-format*)
           *read-default-float-format*))
```

See also [cl-decimals](https://github.com/tlikonen/cl-decimals) to parse and format decimal numbers.

We don't ship *Numcl*, a Numpy clone in Common Lisp, but we invite you to install it right now with Quicklisp:

```lisp
(ql:quickload "numcl")
```

Regular expressions
-------------------

Use `ppcre`.

See <https://common-lisp-libraries.readthedocs.io/cl-ppcre> and <https://lispcookbook.github.io/cl-cookbook/regexp.html>

Threads, monitoring, scheduling
-------------------------------

We ship:

[Bordeaux-Threads](https://common-lisp.net/project/bordeaux-threads/) (`bt` prefix)

[Lparallel](https://lparallel.org/)

[Moira](https://github.com/ruricolist/moira) (monitor and restart background threads)

[trivial-monitored-thread](http://quickdocs.org/trivial-monitored-thread/)

> Trivial Monitored Thread offers a very simple (aka trivial) way of spawning threads and being informed when one any of them crash and die.

[cl-cron](http://quickdocs.org/cl-cron/api) (see the sources on [our fork here](https://github.com/ciel-lang/cl-cron))

For example, run a function every minute:

```lisp
(defun say-hi ()
  (print "Hi!"))
(cl-cron:make-cron-job #'say-hi)
(cl-cron:start-cron)
```

Wait a minute to see some output.

Stop all jobs with `stop-cron`.

`make-cron`'s keyword arguments are:

```lisp
(minute :every) (step-min 1) (hour :every) (step-hour 1) (day-of-month :every)
(step-dom 1) (month :every) (step-month 1) (day-of-week :every)
(step-dow 1)
(boot-only nil) (hash-key nil))
```

HTTP and URI handling
---------------------

See:

-   Dexador. Use the `dex` nickname or the `http` local nickname.
-   Quri
-   Lquery

```lisp
(dex:get "http://my.url")
```

Web
---

We ship:

-   Hunchentoot
-   Easy-routes

<https://lispcookbook.github.io/cl-cookbook/web.html>

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

Types, type checking, exhaustiveness type checking
--------------------------------------------------

From Serapeum, we import:

``` text
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

### More type definitions (trivial-types)

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

Syntax extensions
-----------------

### Arrow macros

We provide the Clojure-like arrow macros and "diamond wands" from the [arrow-macros](https://github.com/hipeta/arrow-macros) library.

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

### Pythonic triple quotes docstring

<https://github.com/smithzvk/pythonic-string-reader>

We can use triple quotes for docstrings, and double quotes within them.

```lisp
(defun foo ()
  """foo "bar"."""
  t)
```

### Lambda shortcuts

You have to enable cl-punch's syntax yourself.

<https://github.com/windymelt/cl-punch/> - Scala-like anonymous lambda literal.

``` example
(cl-punch:enable-punch-syntax)
```

```lisp
;; ^() is converted into (lambda ...) .
;; Each underscore is converted into a lambda argument.

(mapcar ^(* 2 _) '(1 2 3 4 5))
;; => '(2 4 6 8 10)

;; One underscore corresponds one argument.

(^(* _ _) 2 3)
;; => 6

;; <_ reuses last argument.

(mapcar ^(if (oddp _) (* 2 <_) <_) '(1 2 3 4 5))
;; => '(2 2 6 4 10)

;; _! corresponds one argument but it is brought to top of the argument list.
;; It can be useful when you want to change argument order.

(^(cons _ _!) :a :b)
;; => (:b . :a)

(^(list _! _! _!) 1 2 3)
;; => '(3 2 1)
```

Development
-----------

### Testing (Fiveam)

The [FiveAM](https://common-lisp.net/project/fiveam/docs/) test framework is available for use.

Below we create a package to contain our tests and we define the most simple one:

```lisp
(defpackage ciel-5am
  (:use :cl :5am))

(in-package :ciel-5am)

(test test-one
  (is (= 1 1)))
```

Run the test with:

``` txt
(run! 'test-one)

Running test TEST-ONE .
 Did 1 check.
    Pass: 1 (100%)
    Skip: 0 ( 0%)
    Fail: 0 ( 0%)

T
NIL
NIL
```

If the test fails you will see explanations:

``` txt
> (run! 'test-one)

Running test TEST-ONE .f
 Did 2 checks.
    Pass: 1 (50%)
    Skip: 0 ( 0%)
    Fail: 1 (50%)

 Failure Details:
 --------------------------------
 TEST-ONE []:

1

 evaluated to

1

 which is not

=

 to

2


 --------------------------------

NIL
(#<IT.BESE.FIVEAM::TEST-FAILURE {1007307ED3}>)
NIL
```

Use `run` to not print explanations.

You can use `(!)` to re-run the last run test.

You can ask 5am to open the interactive debugger on an error:

``` example
(setf *debug-on-error* t)
```

### Logging (log4cl)

<https://github.com/sharplispers/log4cl/>

``` example
(log:info …)
```

### Discoverability of documentation (repl-utilities' readme, summary,…)

We use `readme` and `summary` from [repl-utilities](http://quickdocs.org/repl-utilities/).

Learn more with:

``` example
(readme repl-utilities)
```

### printv

[printv](https://github.com/danlentz/printv)

```lisp
 (:printv
  (defvar *y*)
  (defparameter *x* 2)
  (setf *y* (sqrt *x*))
  (setf *y* (/ 1 *y*)))

;; This produces the following text to PRINTV's output stream, and still results in the same returned value: 0.70710677.

;;;   (DEFVAR *Y*) => *Y*
;;;   (DEFPARAMETER *X* 2) => *X*
;;;   (SETF *Y* (SQRT *X*)) => 1.4142135
;;;   (SETF *Y* (/ 1 *Y*)) => 0.70710677

```

### Getting a function's arguments list (trivial-arguments)

<https://github.com/Shinmera/trivial-arguments>

```lisp
(defun foo (a b c &optional d) nil)
(arglist #'foo)
;; (a b c &optional d)
```

generic-cl
----------

<https://github.com/alex-gutev/generic-cl/>

todo:

``` example
generic-ciel
```

Example:

```lisp
;; with a struct or class "point":
(defmethod equalp ((p1 point) (p2 point))
   (…))
```

# FAQ

See it here: [FAQ](FAQ.md).

# Final words

That was your life in CL:

<p align="center"><img src="before.jpeg" /></p>
and now:

<p align="center"><img src="after-plus.jpeg" /></p>
