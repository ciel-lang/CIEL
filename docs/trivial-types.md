# Symbols imported from TRIVIAL-TYPES

## ASSOCIATION-LIST-P 


ARGLIST: `(var)`

FUNCTION: Returns true if OBJECT is an association list.

Examples:

    (association-list-p 1) => NIL
    (association-list-p '(1 2 3)) => NIL
    (association-list-p nil) => T
    (association-list-p '((foo))) => T
    (association-list-p '((:a . 1) (:b . 2))) => T
## TYPE-EXPAND 


ARGLIST: `(type-specifier &optional env)`

FUNCTION: Expand TYPE-SPECIFIER in the lexical environment ENV.
## STRING-DESIGNATOR 

## PROPERTY-LIST 


TYPE: Equivalent to `(and list (satisfies
property-list-p))`. VALUE-TYPE is just ignored.

Examples:

    (typep '(:a 1 :b 2) '(property-list integer)) => T
    (typep '(:a 1 :b 2) '(property-list string)) => T
## TUPLE 


ARGLIST: `(&rest args)`

FUNCTION: Exactly same as LIST.

TYPE: Equivalent to `(and list (cons ARG1 (cons ARG2 (cons ARG3 ...))))`
where ARGn is each element of ELEMENT-TYPES.

Examples:

    (typep 1 'tuple) => NIL
    (typep '(1 . 2) 'tuple) => NIL
    (typep '(1 2 3) 'tuple) => NIL
    (typep '(1 2 3) '(tuple integer integer)) => NIL
    (typep '(1 2 3) '(tuple string integer integer)) => NIL
    (typep nil 'tuple) => T
    (typep nil '(tuple)) => T
    (typep '(1 2 3) '(tuple integer integer integer)) => T
## ASSOCIATION-LIST 


TYPE: Equivalent to `(proper-list (cons KEY-TYPE VALUE-TYPE))`. KEY-TYPE
and VALUE-TYPE are just ignored.

Examples:

    (typep '((:a . 1) (:b . 2)) '(association-list integer)) => T
    (typep '((:a . 1) (:b . 2)) '(association-list string)) => T
## CHARACTER-DESIGNATOR 

## PROPERTY-LIST-P 


ARGLIST: `(object)`

FUNCTION: Returns true if OBJECT is a property list.

Examples:

    (property-list-p 1) => NIL
    (property-list-p '(1 2 3)) => NIL
    (property-list-p '(foo)) => NIL
    (property-list-p nil) => T
    (property-list-p '(foo 1)) => T
    (property-list-p '(:a 1 :b 2)) => T
## FILE-ASSOCIATED-STREAM-P 


ARGLIST: `(stream)`

FUNCTION: Returns true if STREAM is a stream associated to a file.
## TYPE-SPECIFIER-P 


ARGLIST: `(type-specifier)`

FUNCTION: Returns true if TYPE-SPECIFIER is a valid type specfiier.
## LIST-DESIGNATOR 

## PACKAGE-DESIGNATOR 

## TUPLEP 


ARGLIST: `(object)`

FUNCTION: Returns true if OBJECT is a tuple, meaning a proper list.

Examples:

    (tuplep 1) => NIL
    (tuplep '(1 . 2)) => NIL
    (tuplep nil) => T
    (tuplep '(1 2 3)) => T
## NON-NIL 


TYPE: Equivalent to `(and (not null) TYPE)` if TYPE is given,
otherwise `(not null)`.

Examples:

    (typep nil '(non-nil symbol)) => NIL
## FILE-ASSOCIATED-STREAM 


TYPE: Equivalent to `(and stream (satisfies file-associated-stream-p))`.
## STREAM-DESIGNATOR 

## FUNCTION-DESIGNATOR 

## FILE-POSITION-DESIGNATOR 

## PATHNAME-DESIGNATOR 

