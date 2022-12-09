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

;; We have access to the STR library:
(print (str:join "-" (list "I" "am" "a" "lisper")))

;; We have access to the DICT notation for hash-tables:
(print "testing dict:")
(print (dict :a 1 :b 2))

;; format! prints on standard output and flushes the streams.
(format! t "cmd?")

;; We can run shell commands:
(cmd:cmd "ls")

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
"I-am-a-lisper"
"testing dict:"

 (dict
  :A 1
  :B 2
 )
cmd? ABOUT.org	    ciel		     ciel-core
   bin  		    docs		     src
 […]
Let's define an alias to run shell commands with '!'. This gives:
/home/vindarel/projets/ciel
ciel-user>
```

## Command line arguments

Access them with `(uiop:command-line-arguments)`.


---

Now, let us iron out the details ;)
