# QSL Built-in symbols

## Both Arduino and Linux (test) build

`nil`

>   The constant false, the empty list, and a symbol.

`t`

>   The constant true, and a symbol.

`(quote e1)`

>   The `quote` special form returns its single argument,
    unevaluated.

`(car v1)`

>   Given a list, `car` returns the first element.  Given a `cons`,
    say the result of `(cons 'x 'y)`, `car` would return `x`.

`(cdr v1)`

>   Given a list, `cdr` returns the rest of the list after removing the
    first element.  Given a `cons`, say the result of `(cons 'x 'y)`,
    `cdr` would return `y`.

`(cons v1 v2)`

>   The `cons` function returns a new dotted-pair with a `car` of `v1`
    and a `cdr` of `v2`.

`(list v1 ...)`

>   Return a newly created list whose `car` is `v1`, and whose `cdr`
    is the list of the remaining arguments.

`(rplaca v1 v2)`

>   Replace the `car` of the `cons` cell `v1` with `v2`.

`(rplacd v1 v2)`

>   Replace the `cdr` of the `cons` cell `v1` with `v2`.


`(+ v1 ...)`

>   Return the sum of the (integer) values given.  If no values are
    given, returns 0.

`(- v1 ...)`

>   If supplied with no values, simply return 0.  If a single value,
    negate it.  If two or more values, return the first minus the sum of
    the rest.

`(* v1 ...)`

>   Return the product of the (integer) values given.  If no values are
    given, returns 1.

`(/ v1 ...)`

>   If supplied with no values, simply return 1.  If a single value,
    return its reciprocal (which is almost always zero).  If two or
    more values, return the first divided by the product of the rest.

`(< v1 v2)`

>   Return `t` if `v1` is less than `v2`, `nil` otherwise.  Note that
    `v1` and `v2` can be both integers or both characters.

`(<= v1 v2)`

>   Return `t` if `v1` is less than or equal to `v2`, `nil` otherwise.
    Note that `v1` and `v2` can be both integers or both characters.

`(> v1 v2)`

>   Return `t` if `v1` is greater than `v2`, `nil` otherwise.  Note that
    `v1` and `v2` can be both integers or both characters.


`(>= v1 v2)`

>   Return `t` if `v1` is greater than or equal to `v2`, `nil`
    otherwise.  Note that `v1` and `v2` can be both integers or both
    characters.


`(= v1 v2)`

>   Return `t` if `v1` is equal to `v2`, `nil` otherwise.  Note that
    `v1` and `v2` can be both integers or both characters.


`(/= v1 v2)`

>   Return `nil` if `v1` is equal to `v2`, `t` otherwise.  Note that
    `v1` and `v2` can be both integers or both characters.


`(progn e1 ...)`

>   Evaluate, in sequence, the expressions `e1`, ..., returning the
    value of the last expression.

`(cond (t1 e1 ...) ...)`

>   Each `cond` clause consists of a test expression, `t1`..., and a
    series of "consequents", `e1`...  Each of the tests is evaluated
    in turn until one is found to be non-`nil`.  The remainder of that
    clause is then executed (as if it were a `progn` body).  The
    return value of `cond` is the value of the last expression
    evaluates (and, therefore, `nil` if all tests fail).

`(while t1 e1 ...)`

>   The test expression, `t1`, is evaluated and, if it returns
    non-`nil`, the remaining expressions, `e1`... are evaluated.  The
    whole is then repeated until the test yields `nil`.

`(setq n1 v1)`

>   The name `n1` is assigned the value `v1`.  This can affect either
    the global binding of `n1` or a local binding, whichever is
    visible.

`(defun n1 (a1 ...) e1 ...)`

>   The name `n1` is globally associated to a function which accepts
    arguments `a1`... and returns the result of evaluating the
    (`progn`-like) body `e1`... in an environment where the argument
    names are bound to the argument values.


`(lambda (a1 ...) e1 ...)`

>   An anonymous function is created, which accepts arguments
    `a1`... and returns the result of evaluating the (`progn`-like)
    body `e1`... in an environment where the argument names are bound
    to the argument values.

`(and e1 ...)`

>   The expressions `e1`, ... are evaluated in turn until either one
    yields `nil` or the series is exhausted.  The return value of
    `and` is the value yielded by the last expression evaluated.

`(or e1 ...)`

>   The expressions `e1`, ... are evaluated in turn until either one
    yields non-`nil` or the series is exhausted.  The return value of
    `or` is the value yielded by the last expression evaluated.


`(let ((n1 v1) ...) e1 ...)`

>   The values `v1`... are evalued, a new environment created in which
    these values are bound to the names `n1`..., then the
    `e1`... evaluated in this environment as if by a `progn`.

`(let* ((n1 v1) ...) e1 ...)`

>   The `let*` special form is just like `let`, except that each of
    the `v...` are evaluated in an environment where the preceding
    `n...` are already bound.

`(apply v1 v2 ... (v8 v9 ...))`

>   The `apply` function takes a "function designator" (a function name
    or a lambda expression) as a first argument, then an arbitrary
    number of arguments to supply to the designated function.  The
    _last_ argument to `apply` must, itself, be a list.

`(read)`

>   Reads a single Lisp expression from the input stream.

`(print v1 ...)`

>   Prints a sequence of expressions to the output stream.

`(readchar)`

>   Reads a single character from the input stream.

`(peekchar)`

>   Returns, but does not remove, the next character from the input
    stream.

`(not v1)`

>   Invert the _logical_ value of `v1`.  `(not nil)` yields `t`.  For
    any other value, `(not v1)` yields `nil`.


`(eq v1 v2)`

>   Returns `t` if `v1` and `v2` are _the same object_, `nil`
    otherwise.

`(neq v1 v2)`

>   Returns `nil` if `v1` and `v2` are _the same object_, `t`
    otherwise.


`(make-string v1)`

>   Return a new string of length `v1`.  All elements of the string
    are initialised to `(code-char 0)`.

`(make-array v1)`

>   Return a new array of length `v1`.  All elements of the array
    are initialised to `nil`.

`(length v1)`

>   Returns the length of a list, or the number of elements in a
    string or an array.

`(aref v1 v2)`

>   Returns the value of the element at position `v2` or the string or
    array `v1`.

`(aset v1 v2 v3)`

>   Modified the string or array `v1` to have the value `v3` at
    position `v2`.

`(char-code v1)`

>   Returns the integer codepoint of the character `v1`.

`(code-char v1)`

>   Returns the character whose codepoint is the integer `v1`.

`(gc)`

>   Forces a garbage collection cycle, and returns the number of bytes
    released into the heap.


`(mem v1)`

>   Returns a single data point about the system memory usage:

        v1 = 0   =>  bytes used by strings and arrays
        v1 = 1   =>  free space in heap
        v1 = 2   =>  number of object headers allocated
        v1 = 10  =>  maximum stack depth used
        v1 = 20  =>  largest number of bytecodes in a compiled expression
        v1 = 21  =>  largest number of constants in a compiled expression

--------------------------------------------------------------------------

## Arduino build only


`(on-tick v1 v2)`

>   Attempt to run the function `v2` every `v1` milliseconds.

`(on-serial v1)`

>   Run the function `v1` every time a character is available on the
    serial port.

`(pin v1 v2)`

>   Set pin `v1` to the Arduino constant `LOW` if `v2` is `nil`, to
    `HIGH` otherwise.


`(wait-for-event)`

>   Put the processor into a sleep state until one of the registered
    events (tick or serial) is ready to run.

`(next-event)`

>   If a registered event is ready, return its function, otherwise
    return `nil`.
