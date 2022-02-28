# Symbols imported from ALEXANDRIA for sequences and hash-tables


## LAST-ELT 

ARGLIST: `(sequence)`

FUNCTION: Returns the last element of SEQUENCE. Signals a type-error if SEQUENCE is
not a proper sequence, or is an empty sequence.

## LENGTH= 

ARGLIST: `(&rest sequences)`

FUNCTION: Takes any number of sequences or integers in any order. Returns true iff
the length of all the sequences and the integers are equal. Hint: there's a
compiler macro that expands into more efficient code if the first argument
is a literal integer.

## RANDOM-ELT 

ARGLIST: `(sequence &key (start 0) end)`

FUNCTION: Returns a random element from SEQUENCE bounded by START and END. Signals an
error if the SEQUENCE is not a proper non-empty sequence, or if END and START
are not proper bounding index designators for SEQUENCE.

## SHUFFLE 

ARGLIST: `(sequence &key (start 0) end)`

FUNCTION: Returns a random permutation of SEQUENCE bounded by START and END.
Original sequence may be destructively modified.
Signals an error if SEQUENCE is not a proper sequence.

## SETP 

ARGLIST: `(object &key (test #'eql) (key #'identity))`

FUNCTION: Returns true if OBJECT is a list that denotes a set, NIL otherwise. A list
denotes a set if each element of the list is unique under KEY and TEST.

## FLATTEN 

ARGLIST: `(tree)`

FUNCTION: Traverses the tree in order, collecting non-null leaves into a list.

## ENSURE-LIST 

ARGLIST: `(list)`

FUNCTION: If LIST is a list, it is returned. Otherwise returns the list designated by LIST.

## ENSURE-CONS 

ARGLIST: `(cons)`

FUNCTION: If CONS is a cons, it is returned. Otherwise returns a fresh cons with CONS
  in the car, and NIL in the cdr.

## DOPLIST 

ARGLIST: `((key val plist &optional values) &body body)`

FUNCTION: Iterates over elements of PLIST. BODY can be preceded by
declarations, and is like a TAGBODY. RETURN may be used to terminate
the iteration early. If RETURN is not used, returns VALUES.

## CIRCULAR-LIST-P 

ARGLIST: `(object)`

FUNCTION: Returns true if OBJECT is a circular list, NIL otherwise.

## CIRCULAR-LIST 

ARGLIST: `(&rest elements)`

FUNCTION: Creates a circular list of ELEMENTS.

TYPE: Type designator for circular lists. Implemented as a SATISFIES type, so not
recommended for performance intensive use. Main usefullness as the
expected-type designator of a TYPE-ERROR.

## PROPER-SEQUENCE 

TYPE: Type designator for proper sequences, that is proper lists and sequences
that are not lists.

## PROPER-LIST-P 

ARGLIST: `(object)`

FUNCTION: Returns true if OBJECT is a proper list.

## PROPER-LIST 

TYPE: Type designator for proper lists. Implemented as a SATISFIES type, hence
not recommended for performance intensive use. Main usefullness as a type
designator of the expected type in a TYPE-ERROR.

## IOTA 

ARGLIST: `(n &key (start 0) (step 1))`

FUNCTION: Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1.

Examples:

  (iota 4)                      => (0 1 2 3)
  (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
  (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)

## HASH-TABLE-KEYS 

ARGLIST: `(table)`

FUNCTION: Returns a list containing the keys of hash table TABLE.

## HASH-TABLE-VALUES 

ARGLIST: `(table)`

FUNCTION: Returns a list containing the values of hash table TABLE.

## ENSURE-GETHASH 

ARGLIST: `(key hash-table &optional default)`

FUNCTION: Like GETHASH, but if KEY is not found in the HASH-TABLE saves the DEFAULT
under key before returning it. Secondary return value is true if key was
already in the table.
