(in-package :sbcli)

(defun last-nested-expr (s/sexp)
  "From an input with nested parens (or none), return the most nested
function call (or the first thing at the prompt).

(hello (foo (bar:qux *zz* ?
=>
bar:qux
"
  (let* ((input (str:trim s/sexp))
         (last-parens-token (first (last (str:split #\( input)))))
    (first (str:words last-parens-token))))

#+or(nil)
(progn
  (assert (string= "baz:qux"
                   (last-nested-expr "(hello (foo bar (baz:qux zz ?")))
  (assert (string= "baz:qux"
                   (last-nested-expr "(baz:qux zz ?")))
  (assert (string= "qux"
                   (last-nested-expr "(baz (qux ?")))
  (assert (string= "sym"
                   (last-nested-expr "sym ?"))))
