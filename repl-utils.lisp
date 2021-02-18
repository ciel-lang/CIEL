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

;;;;
;;;; Syntax highlighting if pygments is installed.
;;;;
(defun maybe-highlight (str)
  (if *syntax-highlighting*
      (let ((pygmentize (or *pygmentize*
                            (which:which "pygmentize"))))
        (when pygmentize
          (with-input-from-string (s str)
            (let ((proc (uiop:launch-program (alexandria:flatten
                                              (list pygmentize *pygmentize-options*))
                                             :input s
                                             :output :stream)))
              (read-line (uiop:process-info-output proc) nil "")))))
    str))

(defun syntax-hl ()
  (rl:redisplay)
  (let ((res (maybe-highlight rl:*line-buffer*)))
    (format t "~c[2K~c~a~a~c[~aD" #\esc #\return rl:*display-prompt* res #\esc (- rl:+end+ rl:*point*))
    (when (= rl:+end+ rl:*point*)
      (format t "~c[1C" #\esc))
    (finish-output)))

