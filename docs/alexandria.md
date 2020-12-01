# Symbols imported from ALEXANDRIA for sequences and lists

## IOTA 


ARGLIST: `(n &key (start 0) (step 1))`

FUNCTION: Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1.

Examples:

  (iota 4)                      => (0 1 2 3)
  (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
  (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)
## PROPER-LIST 


TYPE: Type designator for proper lists. Implemented as a SATISFIES type, hence
not recommended for performance intensive use. Main usefullness as a type
designator of the expected type in a TYPE-ERROR.
## PROPER-LIST-P 


ARGLIST: `(object)`

FUNCTION: Returns true if OBJECT is a proper list.
## PROPER-SEQUENCE 


TYPE: Type designator for proper sequences, that is proper lists and sequences
that are not lists.
## CIRCULAR-LIST 


ARGLIST: `(&rest elements)`

FUNCTION: Creates a circular list of ELEMENTS.

TYPE: Type designator for circular lists. Implemented as a SATISFIES type, so not
recommended for performance intensive use. Main usefullness as the
expected-type designator of a TYPE-ERROR.
## CIRCULAR-LIST-P 


ARGLIST: `(object)`

FUNCTION: Returns true if OBJECT is a circular list, NIL otherwise.
## DOPLIST 


ARGLIST: `((key val plist &optional values) &body body)`

FUNCTION: Iterates over elements of PLIST. BODY can be preceded by
declarations, and is like a TAGBODY. RETURN may be used to terminate
the iteration early. If RETURN is not used, returns VALUES.
## ENSURE-CONS 


ARGLIST: `(cons)`

FUNCTION: If CONS is a cons, it is returned. Otherwise returns a fresh cons with CONS
  in the car, and NIL in the cdr.
## ENSURE-LIST 


ARGLIST: `(list)`

FUNCTION: If LIST is a list, it is returned. Otherwise returns the list designated by LIST.
## FLATTEN 


ARGLIST: `(tree)`

FUNCTION: Traverses the tree in order, collecting non-null leaves into a list.
## SETP 


ARGLIST: `(object &key (test #'eql) (key #'identity))`

FUNCTION: Returns true if OBJECT is a list that denotes a set, NIL otherwise. A list
denotes a set if each element of the list is unique under KEY and TEST.
## EMPTYP 


ARGLIST: `(sequence)`

FUNCTION: Returns T if SEQUENCE is an empty sequence and NIL
   otherwise. Signals an error if SEQUENCE is not a sequence.
## SHUFFLE 


ARGLIST: `(sequence &key (start 0) end)`

FUNCTION: Returns a random permutation of SEQUENCE bounded by START and END.
Original sequence may be destructively modified, and (if it contains
CONS or lists themselv) share storage with the original one.
Signals an error if SEQUENCE is not a proper sequence.
## RANDOM-ELT 


ARGLIST: `(sequence &key (start 0) end)`

FUNCTION: Returns a random element from SEQUENCE bounded by START and END. Signals an
error if the SEQUENCE is not a proper non-empty sequence, or if END and START
are not proper bounding index designators for SEQUENCE.
## LENGTH= 


ARGLIST: `(&rest sequences)`

FUNCTION: Takes any number of sequences or integers in any order. Returns true iff
the length of all the sequences and the integers are equal. Hint: there's a
compiler macro that expands into more efficient code if the first argument
is a literal integer.
## LAST-ELT 


ARGLIST: `(sequence)`

FUNCTION: Returns the last element of SEQUENCE. Signals a type-error if SEQUENCE is
not a proper sequence, or is an empty sequence.
