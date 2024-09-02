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
  ;; ARG can be any string. This fails with "(qs:?" (note the "?").
  (ignore-errors
    (when arg
      (namestring (pathname-name arg)))))

(defun shell-command-wrapper-p (command)
  "Is this command (string) a shell wrapper?"
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

(defun %visual-command-p (command)
  ;; probably from shlex.
  (setf command (shell-ensure-clean-command-list command))
  (let ((cmd (shell-first-positional-argument command)))
    (when cmd
      (find (basename cmd)
            *visual-commands*
            :test #'string=))))

(defun visual-command-p (command)
  "Return true if COMMAND starts with '!' (clesh syntax)
  and runs one of the programs in `*visual-commands*'.

  COMMAND is either a list of strings or a string. `*command-wrappers*' are supported, i.e. the following works:

  env FOO=BAR sudo -i powertop

  Changed  <2024-09-02>: shell commands must always start with a !, following the clesh syntax, that is enabled by default."
  (and (str:starts-with-p "!" command)
       ;; The shell lexer can fail, the top level would catch the error
       ;; and we'll see like:
       ;; !echo "
       ;; Error: Missing closing quotation in string
       (%visual-command-p command)))

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
