# Scripting

> Note: this is brand new! Expect limitations and changes.

Get the `ciel` binary and call it with a .lisp file:

```
$ ciel script.lisp
```

An example script:

```lisp
;; Start your script with this to access all CIEL goodies:
(in-package :ciel-user)

(defun hello (name)
  "Say hello."
  ;; format! prints on standard output and flushes the streams.
  (format! t "Hello ~a!~&" name))

;; Access CLI args:
(hello (second (uiop:command-line-arguments)))

;; We have access to the DICT notation for hash-tables:
(print "testing dict:")
(print (dict :a 1 :b 2))

;; We can run shell commands:
(cmd:cmd "ls")

;; Access environment variables:
(hello (os:getenv "USER"))  ;; os is a nickname for uiop/os

(format! t "Let's define an alias to run shell commands with '!'. This gives: ")
(defalias ! #'cmd:cmd)
(! "pwd")

;; In cas of an error, we can ask for a CIEL toplevel REPL:
(handler-case
    (error "oh no")
  (error (c)
    (format! t "An error occured: ~a" c)
    (format! t "Here's a CIEL top level REPL: ")
    (sbcli::repl :noinform t)))
```

Output:


```
$ ciel myscript.lisp you
=>

Hello you!
"testing dict:"

 (dict
  :A 1
  :B 2
 )
cmd? ABOUT.org	    ciel		     ciel-core
   bin  		    docs		     src
 […]
Hello vindarel!
Let's define an alias to run shell commands with '!'. This gives:
/home/vindarel/projets/ciel
ciel-user>
```

## Command line arguments

Access them with `(uiop:command-line-arguments)`.


---

Now, let us iron out the details ;)
