QSL Types
=========

The following types are implemented by QSL.

character
---------

Characters are stored as 8-bit values.  (In fact, they aren't "stored"
at all, but are efficiently encoded as special object references.)

The notation for characters is stolen from Emacs Lisp: `?x` is what
Common Lisp would call `#\x`.  Note, though, that no special syntax is
provided for non-printing characters.

integer
-------

Integers are 32-bit signed quantities, giving a range of
-2147483648..2147483647.  Arithmetic outside this range will wrap.
(Like characters, small integers (-32768..32767) are encoded as
special object references for efficiency.)

No bases other than decimal are supported.

string
------

A string is a sequence of 0..255 characters.

Strings are written between matching `"` characters.  If a string should
contain a `"` or a `\`, it must be escaped by preceding it with a
`\`.  Anything else is acceptable.  Again, there's no special syntax
for non-printing characters.

array
-----

An array is a sequence of object references.  There is no literal
notation for arrays.

symbol
------

A symbol is typically used to name a variable or a function.  Two symbols,
`t` and `nil`, are constants.  All other symbols can be used as global
or local variable names.  Global variables are initialised to `nil`.
Note that QSL is a "lisp-2": the same symbol may name both a variable
and a function without conflict.

Anything read by QSL that can't be interpreted in any other way is
taken to be a symbol, meaning things like `1+` and `...---...` are
valid function names.  If you need even weirder symbols, you can quote
them just like strings, but using `|` instead of `"`.

cons
----

A cons cell, also known as a "dotted pair", is like an array holding
precisely two object references, known as the `car` and the `cdr`.

A cell with a `car` of X and a `cdr` of Y can be written `(X . Y)`.

A series of cons cells, linked through their `cdr` pointers, is how
Lisp creates a list.  If you write

    '(1 2 3 4)

this is stored internally as

    '(1 . (2 . (3 . (4 . nil))))

lambda expression
-----------------

A lambda expression is an anonymous function.  They are created by
evaluating a `lambda` special form and must, eventually, be fed to the
`apply` function to be at all useful:

    (apply
      (lambda (a b) (+ (* a a) (* b b)))
      '(3 4))
    =25

named function
--------------

A named function is usually preferred to a lambda expression.

    (defun sum-of-squares (a b)
      (+ (* a a) (* b b)))
    (sum-of-squares 3 4)
    =25

All named functions are global.
