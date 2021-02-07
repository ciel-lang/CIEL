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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run visual / interactive / ncurses commands in their terminal window.
;;;
;;; How to guess a program is interactive?
;;; We currently look from a hand-made list (à la Eshell).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *visual-commands*
  '(;; "emacs -nw" ;; in eshell, concept of visual-subcommands.
    "vim" "vi"
    "nano"
    "htop" "top"
    "man" "less" "more"
    "screen" "tmux"
    "lynx" "links" "mutt" "pine" "tin" "elm" "ncftp" "ncdu"
    "ranger"
    ;; last but not least
    "ciel-repl")
  "List of visual/interactive/ncurses-based programs that will be run in their own terminal window.")

(defparameter *visual-terminal-emulator-choices*
  '("terminator" "x-terminal-emulator" "xterm" "gnome-terminal"))

(defparameter *visual-terminal-switches* '("-e")
  "Default options to the terminal. `-e' aka `--command'.")

(defun find-terminal ()
  "Return the first terminal emulator found on the system from the `*visual-terminal-emulator-choices*' list."
  (loop for program in *visual-terminal-emulator-choices*
     when (which:which program)
     return program))

(defun visual-command-p (text)
  "The command TEXT starts by a known visual command, listed in `*visual-commands*'."
  (let* ((cmd (string-left-trim "!" text)) ;; strip clesh syntax.
         (first-word (first (str:words cmd))))
    ;; This will be smarter. https://github.com/ruricolist/cmd/issues/10
    (find first-word *visual-commands* :test #'equalp)))

(defun run-visual-command (text)
  "Run this text command into another terminal window."
  (let ((cmd (string-left-trim "!" text)))
    (uiop:launch-program `( ,(find-terminal)
                             ;; quick way to flatten the list of switches:
                             ,@*visual-terminal-switches*
                             ,cmd))))
