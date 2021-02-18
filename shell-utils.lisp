(in-package :sbcli)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Run visual / interactive / ncurses commands in their terminal window.
;;;
;;; How to guess a program is interactive?
;;; We currently look from a hand-made list (à la Eshell).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; thanks @ambrevar: https://github.com/ruricolist/cmd/issues/10
;; for handling command wrappers (sudo) and vterm.

(defparameter *visual-commands*
  '(;; "emacs -nw" ;; in eshell, see concept of visual-subcommands.
    "vim" "vi"
    "nano"
    "htop" "top"
    "man" "less" "more"
    "screen" "tmux"
    "lynx" "links" "mutt" "pine" "tin" "elm" "ncftp" "ncdu"
    "ranger"
    "mpv" "mplayer"
    "ipython" "irb" "iex"               ;; TBC
    ;; last but not least
    "ciel-repl")
  "List of visual/interactive/ncurses-based programs that will be run in their own terminal window.")

(defun vterm-terminal (cmd)
  "Build a command (string) to send to emacsclient to open CMD with Emacs' vterm."
  (list
   "emacsclient" "--eval"
   (let ((*print-case* :downcase))
     (write-to-string
      `(progn
         (vterm)
         (vterm-insert ,cmd)
         (vterm-send-return))))))

(defparameter *visual-terminal-emulator-choices*
  '("terminator" "x-terminal-emulator" "xterm" "gnome-terminal"
    #'vterm-terminal)
  "List of terminals, either a string or a function (that returns a more complete command, as a string).")

(defparameter *visual-terminal-switches* '("-e")
  "Default options to the terminal. `-e' aka `--command'.")

(defvar *command-wrappers* '("sudo" "env"))

(defun find-terminal ()
  "Return the first terminal emulator found on the system from the `*visual-terminal-emulator-choices*' list."
  (loop for program in *visual-terminal-emulator-choices*
     if (and (stringp program)
             (which:which program))
     return program
     else if (functionp program) return program))

(defun basename (arg)
  (when arg
    (namestring (pathname-name arg))))

(defun shell-command-wrapper-p (command)
  (find (basename command)
        *command-wrappers*
        :test #'string-equal))

(defun shell-flag-p (arg)
  (str:starts-with-p "-" arg))

(defun shell-variable-p (arg)
  (and (< 1 (length arg))
       (str:contains? "=" (subseq arg 1))))

(defun shell-first-positional-argument (command)
  "Recursively find the first command that's not a flag, not a variable setting and
not in `*command-wrappers*'."
  (when command
    (if (or (shell-flag-p (first command))
            (shell-variable-p (first command))
            (shell-command-wrapper-p (first command)))
        (shell-first-positional-argument (rest command))
        (first command))))

(defun shell-ensure-clean-command-list (command)
  "Return a list of commands, stripped out of a potential \"!\" prefix from Clesh syntax."
  (unless (consp command)
    (setf command (shlex:split command)))
  ;; remove optional ! clesh syntax.
  (setf (first command)
        (string-left-trim "!" (first command)))
  ;; remove blank strings, in case we wrote "! command".
  (remove-if #'str:blankp command))

(defun visual-command-p (command)
  "Return true if COMMAND runs one of the programs in `*visual-commands*'.
  COMMAND is either a list of strings or a string.
`*command-wrappers*' are supported, i.e. the following works:

  env FOO=BAR sudo -i powertop"
  (setf command (shell-ensure-clean-command-list command))
  (let ((cmd (shell-first-positional-argument command)))
    (when cmd
      (find (basename cmd)
            *visual-commands*
            :test #'string=))))

(defparameter *lisp-symbol-identifiers* (list #\( #\* #\# #\: #\-))

(defun lisp-command-p (text)
  "Is considered a lisp command if it starts with a *lisp-symbol-identifiers* character.
  If it starts with a parenthesis, it is sure a lisp command."
  (let ((tokens (shlex:split text)))
    (cond
      ;; Starts with a ( => lisp, definitely.
      ((str:starts-with-p "(" text)
       t)
      ;; Starts with a special character: lisp.
      ((some (lambda (char)
               (str:starts-with-p (string char) text))
             *lisp-symbol-identifiers*)
       t)

      ;; We have one token that contains a ":": lisp symbol.
      ((and (= 1 (length tokens))
            (str:contains? ":" text))
       t)

      ;; We have space-separated tokens, but the command doesn't start with a paren: NOT lisp.
      ;; Exple:
      ;; uiop featurep
      ;; is not (uiop:featurep) and is a shell command.
      ((and (not (str:starts-with-p "(" text))
            (< 1 (length tokens)))
       nil)

      ;; If we didn't recognize lisp here: it's a shell command.
      (t
       ;; (format t "default: shell command!~&")
       nil))))

#+(or)
(progn
  (assert (lisp-command-p "(uiop:featurep"))
  (assert (lisp-command-p "uiop:*foo*"))
  (assert (lisp-command-p "*foo"))
  (assert (lisp-command-p "#foo"))
  (assert (lisp-command-p "-foo"))
  (assert (not (lisp-command-p "uiop featurep")))
  (assert (not (lisp-command-p "ls")))
  (assert (not (lisp-command-p "ls -l")))
  (assert (not (lisp-command-p "foo --arg=1:1"))))

(defun run-shell-command (text)
  "Run this shell command."
  ;; XXX: not with Clesh = difference in behaviours coming.
  (ignore-errors
    (cmd:cmd text)))

(defun run-visual-command (text)
  "Run this command (string) in another terminal window."
  (let* ((cmd (string-left-trim "!" text))
         (terminal (find-terminal)))
    (if (str:emptyp terminal)
        (format *error-output* "Could not find a terminal emulator amongst the list ~a: ~s"
                '*visual-terminal-emulator-choices*
                *visual-terminal-emulator-choices*)
        (cond
          ((stringp terminal)
           (uiop:launch-program `(,terminal
                                  ;; flatten the list of switches
                                  ,@*visual-terminal-switches*
                                  ,cmd)))
          ((functionp terminal)
           (uiop:launch-program (funcall terminal cmd)))
          (t
           (format *error-output* "We cannot use a terminal designator of type ~a. Please use a string (\"xterm\") or a function that returns a string." (type-of terminal)))))))

#+(or)
(assert (string-equal "htop"
                      (visual-command-p "env rst=ldv sudo htop")))

(defun maybe-run-visual-command (cmd)
  (if (visual-command-p cmd)
      (run-visual-command cmd)
      (run-shell-command cmd)))
