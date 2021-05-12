# Symbols imported from SERAPEUM

## ASSORT 


ARGLIST: `(seq &key (key #'identity) (test #'eql) (start 0) end)`

FUNCTION: Return SEQ assorted by KEY.

     (assort (iota 10)
             :key (lambda (n) (mod n 3)))
     => '((0 3 6 9) (1 4 7) (2 5 8))

Groups are ordered as encountered. This property means you could, in
principle, use `assort' to implement `remove-duplicates' by taking the
first element of each group:

     (mapcar #'first (assort list))
     ≡ (remove-duplicates list :from-end t)

However, if TEST is ambiguous (a partial order), and an element could
qualify as a member of more than one group, then it is not guaranteed
that it will end up in the leftmost group that it could be a member
of.

    (assort '(1 2 1 2 1 2) :test #'<=)
    => '((1 1) (2 2 1 2))
## BATCHES 


ARGLIST: `(seq n &key (start 0) end even)`

FUNCTION: Return SEQ in batches of N elements.

    (batches (iota 11) 2)
    => ((0 1) (2 3) (4 5) (6 7) (8 9) (10))

If EVEN is non-nil, then SEQ must be evenly divisible into batches of
size N, with no leftovers.
## IOTA 


ARGLIST: `(n &key (start 0) (step 1))`

FUNCTION: Return a list of n numbers, starting from START (with numeric contagion
from STEP applied), each consequtive number being the sum of the previous one
and STEP. START defaults to 0 and STEP to 1.

Examples:

  (iota 4)                      => (0 1 2 3)
  (iota 3 :start 1 :step 1.0)   => (1.0 2.0 3.0)
  (iota 3 :start -1 :step -1/2) => (-1 -3/2 -2)
## RUNS 


ARGLIST: `(seq &key (start 0) end (key #'identity) (test #'eql))`

FUNCTION: Return a list of runs of similar elements in SEQ.
The arguments START, END, and KEY are as for `reduce'.

    (runs '(head tail head head tail))
    => '((head) (tail) (head head) (tail))

The function TEST is called with the first element of the run as its
first argument.

    (runs '(1 2 3 1 2 3) :test #'<)
    => ((1 2 3) (1 2 3))
## PARTITION 


ARGLIST: `(pred seq &key (start 0) end (key #'identity))`

FUNCTION: Partition elements of SEQ into those for which PRED returns true
and false.

Return two values, one with each sequence.

Exactly equivalent to:
     (values (remove-if-not predicate seq) (remove-if predicate seq))
except it visits each element only once.

Note that `partition` is not just `assort` with an up-or-down
predicate. `assort` returns its groupings in the order they occur in
the sequence; `partition` always returns the “true” elements first.

    (assort '(1 2 3) :key #'evenp) => ((1 3) (2))
    (partition #'evenp '(1 2 3)) => (2), (1 3)
## PARTITIONS 


ARGLIST: `(preds seq &key (start 0) end (key #'identity))`

FUNCTION: Generalized version of PARTITION.

PREDS is a list of predicates. For each predicate, `partitions'
returns a filtered copy of SEQ. As a second value, it returns an extra
sequence of the items that do not match any predicate.

Items are assigned to the first predicate they match.
## SPLIT-SEQUENCE 


ARGLIST: `(delimiter sequence &key (start 0) (end nil) (from-end nil) (count nil)
 (remove-empty-subseqs nil) (test #'eql test-p) (test-not nil test-not-p)
 (key #'identity))`

FUNCTION: Return a list of subsequences in seq delimited by delimiter.
If :remove-empty-subseqs is NIL, empty subsequences will be included
in the result; otherwise they will be discarded.  All other keywords
work analogously to those for CL:SUBSTITUTE.  In particular, the
behaviour of :from-end is possibly different from other versions of
this function; :from-end values of NIL and T are equivalent unless
:count is supplied. :count limits the number of subseqs in the main
resulting list. The second return value is an index suitable as an
argument to CL:SUBSEQ into the sequence indicating where processing
stopped.
## COUNT-CPUS 


ARGLIST: `(&key (default 2))`

FUNCTION: Try very hard to return a meaningful count of CPUs.

The second value is T if the number of processors could be queried,
`nil' otherwise.
## DICT 


ARGLIST: `(&rest keys-and-values)`

FUNCTION: A concise constructor for hash tables.

    (gethash :c (dict :a 1 :b 2 :c 3)) => 3, T

By default, return an 'equal hash table containing each successive
pair of keys and values from KEYS-AND-VALUES.

If the number of KEYS-AND-VALUES is odd, then the first argument is
understood as the test.

     (gethash "string" (dict "string" t)) => t
     (gethash "string" (dict 'eq "string" t)) => nil

Note that `dict' can also be used for destructuring (with Trivia).

    (match (dict :x 1)
      ((dict :x x) x))
    => 1
## DO-HASH-TABLE 


ARGLIST: `((key value table &optional return) &body body)`

FUNCTION: Iterate over hash table TABLE, in no particular order.

At each iteration, a key from TABLE is bound to KEY, and the value of
that key in TABLE is bound to VALUE.
## DICT* 


ARGLIST: `(dict &rest args)`

FUNCTION: Merge new bindings into DICT.
Roughly equivalent to `(merge-tables DICT (dict args...))'.
## DICTQ 


ARGLIST: `(&rest keys-and-values)`

FUNCTION: A literal hash table.
Like `dict', but the keys and values are implicitly quoted, and the
hash table is inlined as a literal object.
## POPHASH 


ARGLIST: `(key hash-table)`

FUNCTION: Lookup KEY in HASH-TABLE, return its value, and remove it.

This is only a shorthand. It is not in itself thread-safe.

From Zetalisp.
## SWAPHASH 


ARGLIST: `(key value hash-table)`

FUNCTION: Set KEY and VALUE in HASH-TABLE, returning the old values of KEY.

This is only a shorthand. It is not in itself thread-safe.

From Zetalisp.
## HASH-FOLD 


ARGLIST: `(fn init hash-table)`

FUNCTION: Reduce TABLE by calling FN with three values: a key from the hash
table, its value, and the return value of the last call to FN. On the
first call, INIT is supplied in place of the previous value.

From Guile.
## MAPHASH-RETURN 


ARGLIST: `(fn hash-table)`

FUNCTION: Like MAPHASH, but collect and return the values from FN.
From Zetalisp.
## MERGE-TABLES 


ARGLIST: `(&rest tables)`

FUNCTION: Merge TABLES, working from left to right.
The resulting hash table has the same parameters as the first table.

If no tables are given, an new, empty hash table is returned.

If a single table is given, a copy of it is returned.

If the same key is present in two tables, the value from the rightmost
table is used.

All of the tables being merged must have the same value for
`hash-table-test'.

Clojure's `merge'.
## FLIP-HASH-TABLE 


ARGLIST: `(table &key (test (constantly t)) (key #'identity))`

FUNCTION: Return a table like TABLE, but with keys and values flipped.

     (gethash :y (flip-hash-table (dict :x :y)))
     => :x, t

TEST allows you to filter which keys to set.

     (def number-names (dictq 1 one 2 two 3 three))

     (def name-numbers (flip-hash-table number-names))
     (def name-odd-numbers (flip-hash-table number-names :filter #'oddp))

     (gethash 'two name-numbers) => 2, t
     (gethash 'two name-odd-numbers) => nil, nil

KEY allows you to transform the keys in the old hash table.

     (def negative-number-names (flip-hash-table number-names :key #'-))
     (gethash 'one negative-number-names) => -1, nil

KEY defaults to `identity'.
## SET-HASH-TABLE 


ARGLIST: `(set &rest hash-table-args &key (test #'eql) (key #'identity) (strict t)
     &allow-other-keys)`

FUNCTION: Return SET, a list considered as a set, as a hash table.
This is the equivalent of Alexandria's `alist-hash-table' and
`plist-hash-table' for a list that denotes a set.

STRICT determines whether to check that the list actually is a set.

The resulting hash table has the elements of SET for both its keys and
values. That is, each element of SET is stored as if by
     (setf (gethash (key element) table) element)
## HASH-TABLE-PREDICATE 


ARGLIST: `(hash-table)`

FUNCTION: Return a predicate for membership in HASH-TABLE.
The predicate returns the same two values as `gethash', but in the
opposite order.
## HASH-TABLE-FUNCTION 


ARGLIST: `(hash-table &key read-only strict (key-type 't) (value-type 't) strict-types)`

FUNCTION: Return a function for accessing HASH-TABLE.

Calling the function with a single argument is equivalent to `gethash'
against a copy of HASH-TABLE at the time HASH-TABLE-FUNCTION was
called.

    (def x (make-hash-table))

    (funcall (hash-table-function x) y)
    ≡ (gethash y x)

If READ-ONLY is nil, then calling the function with two arguments is
equivalent to `(setf (gethash ...))' against HASH-TABLE.

If STRICT is non-nil, then the function signals an error if it is
called with a key that is not present in HASH-TABLE. This applies to
setting keys, as well as looking them up.

The function is able to restrict what types are permitted as keys and
values. If KEY-TYPE is specified, an error will be signaled if an
attempt is made to get or set a key that does not satisfy KEY-TYPE. If
VALUE-TYPE is specified, an error will be signaled if an attempt is
made to set a value that does not satisfy VALUE-TYPE. However, the
hash table provided is *not* checked to ensure that the existing
pairings KEY-TYPE and VALUE-TYPE -- not unless STRICT-TYPES is also
specified.
## MAKE-HASH-TABLE-FUNCTION 


ARGLIST: `(&rest args &key &allow-other-keys)`

FUNCTION: Call `hash-table-function' on a fresh hash table.
ARGS can be args to `hash-table-function' or args to
`make-hash-table', as they are disjoint.
## DELETE-FROM-HASH-TABLE 


ARGLIST: `(table &rest keys)`

FUNCTION: Return TABLE with KEYS removed (as with `remhash').
Cf. `delete-from-plist' in Alexandria.
## PAIRHASH 


ARGLIST: `(keys data &optional hash-table)`

FUNCTION: Like `pairlis', but for a hash table.

Unlike `pairlis', KEYS and DATA are only required to be sequences, not
lists.

By default, the hash table returned uses `eql' as its tests. If you
want a different test, make the table yourself and pass it as the
HASH-TABLE argument.
