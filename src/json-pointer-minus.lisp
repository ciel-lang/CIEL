;;
;; JSON-POINTER
;;
;; cl-json-pointer has lengthy functions: get-by-json-pointer, add-by-json-pointer, etc.
;; Let's create shorter ones: get-by etc.
;;
;; update: oops, they have the cl-json-pointer/synonyms system with even shorter "get", "set" etc.
;; Let's use it.
;;
;; But why doesn't it accept a JSON string as input?!

(in-package :ciel)

(setf cl-json-pointer:*json-object-flavor* :shasht)

(defun json-pointer-get-by (obj pointer &key (flavor cl-json-pointer:*json-object-flavor*))
  "Traverse OBJ with POINTER and return three values:

- the found value (`nil' if not found),
- a generalized boolean saying the existence of the place pointed by POINTER,
- and NIL.

GET-BY is a shorter name of cl-json-pointer:get-by-json-pointer added by CIEL.

In CIEL, we use the SHASHT library to handle JSON.
SHASHT returns a hash-table.
JSON-POINTER functions take a dict (hash-table) as first argument.

Examples:

(json-pointer:get-by (dict \"a\"
                       (dict \"aa\" 11))
                     \"/a/aa\")
;; => 11

Parse a JSON string with SHASHT:READ-JSON before feeding the result to json-pointer:

(defvar *json-string*  \"{\\\"foo\\\": [\\\"1\\\", \\\"2\\\"]}\")

(let ((obj (shasht:read-json *json-string*)))
   (json-pointer:get-by obj \"/foo\"))
;; =>
#(\"1\" \"2\")
T
NIL
"
  (cl-json-pointer:get-by-json-pointer obj pointer :flavor flavor))

#+(or)
(defvar *json-string*  "{\"foo\": [\"1\", \"2\"]}")

(defun json-pointer-set-by (obj pointer value &key (flavor cl-json-pointer:*json-object-flavor*))
  "Traverse OBJ with POINTER, set VALUE into the pointed place, and return the modified OBJ."
  (cl-json-pointer:set-by-json-pointer obj pointer value :flavor flavor))

(defun json-pointer-update-by (place pointer value &key (flavor cl-json-pointer:*json-object-flavor*))
  "Set the result of SET-BY to the referred PLACE.

UPDATE-BY is a modify macro for SET-BY (like PUSH or INCF)."
  (cl-json-pointer:update-by-json-pointer place pointer value :flavor flavor))

(defun json-pointer-add-by (obj pointer value &key (flavor cl-json-pointer:*json-object-flavor*))
  "Works as `set-by', except this tries to make a new list when setting to lists."
  (cl-json-pointer:add-by-json-pointer obj pointer value :flavor flavor))

(defun json-pointer-delete-by (obj pointer &key (flavor cl-json-pointer:*json-object-flavor*))
  "Traverse OBJ with POINTER, delete the pointed place, and return the modified OBJ."
  (cl-json-pointer:delete-by-json-pointer obj pointer :flavor flavor))

(defun json-pointer-deletef-by (place pointer &key (flavor cl-json-pointer:*json-object-flavor*))
  "Set the result of DELETE-BY to the referred PLACE.

This is a modify macro for DELETE-BY (like PUSH or INCF)."
  (cl-json-pointer:deletef-by-json-pointer place pointer :flavor flavor))

(defun json-pointer-remove-by (obj pointer &key (flavor cl-json-pointer:*json-object-flavor*))
  "Like `delete-by', except this tries to make a new list when deleting from lists."
  (cl-json-pointer:remove-by-json-pointer obj pointer :flavor flavor))

(defun json-pointer-exists-p-by (obj pointer &key (flavor cl-json-pointer:*json-object-flavor*))
  "Traverse OBJ with POINTER and return the existence of the place pointed by POINTER."
  (cl-json-pointer:exists-p-by-json-pointer obj pointer :flavor flavor))

(defun json-pointer-shorten-functions ()
  "Shorten function names inside the cl-json-pointer package.
  (not so useful anymore: they have shorter synonyms: get, set etc)"
  (let ((tuples (list
                 (list "GET-BY"  #'json-pointer-get-by)
                 ;;     NAME     NEW FUNCTION
                 (list "ADD-BY" #'json-pointer-add-by)
                 (list "SET-BY" #'json-pointer-set-by)
                 (list "UPDATE-BY" #'json-pointer-update-by)
                 (list "DELETE-BY" #'json-pointer-delete-by)
                 (list "DELETEF-BY" #'json-pointer-deletef-by)
                 (list "REMOVE-BY" #'json-pointer-remove-by)
                 (list "EXISTS-P-BY"  #'json-pointer-exists-p-by))))
    (loop for tuple in tuples
          for sym = (intern (first tuple) 'cl-json-pointer)
          do (export sym 'cl-json-pointer)
             (setf (symbol-function sym) (second tuple))
          collect sym)))

(json-pointer-shorten-functions)
