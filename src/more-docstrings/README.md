
Augment the docstring of built-in functions and macros with more explanations and examples.

Let's see how useful it is for newcomers.

This is part of CIEL (CIEL Is an Extended Lisp).

A few functions are done:

- `loop`: no docstring by default. It becomes

```
The basic LOOP structure is

(loop for x in (list x y z)
   do …)

"do" is for side effects.

Use "collect" to return results:

(loop for x in (list 1 2 3)
  collect (* x 10))

To iterate over arrays, use "across" instead of "in".

To iterate over hash-tables… try MAPHASH first :D

For many examples, see the CL Cookbook:
https://lispcookbook.github.io/cl-cookbook/iteration.html
```

- `mapcar`, `maphash`
- `sort`

and that's it. To be continued.

A gotcha though:

- we are modifying the symbols in the :cl package, there is currently no way to "undo" these additions.
