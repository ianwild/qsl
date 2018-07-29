Tuning QSL
==========

When working with the Arduino, it is important to keep the memory
footprint as small as possible.  On the other hand, reducing memory
too much will restrict what can be done in QSL.

In `buffer_limits.h` are a number of constants that control how much
memory is allocated for the various "hidden" things QSL needs.

`TOTAL_HEAP_SIZE` is the memory QSL sets aside for its workspace.  It
also needs a few bytes for random C-level variables and the C stack.  On an
ATmega2560 you can easily put this up to 7K, which should allow quite
complex Lisp programs.  You'll get a compile-time error if you try to use a
heap of less than 768 bytes.

To read _anything_, QSL needs to allocate a buffer of
`MAX_TOKEN_LENGTH` bytes, but this buffer is freed once a token is
read, so it's not a disaster if you're too generous here.

To compile an expression a buffer of `MAX_OPCODES_PER_LAMBDA` bytes is
needed to store the bytecodes being created, and one of
`MAX_LITERALS_PER_LAMBDA`*2 bytes for the object indices referenced by
the expression.  Once the expression is compiled, new (correctly
sized) byte- and object- vectors are allocated, and the temporary
`MAX_*` versions freed so, again, generosity will not be penalised too
badly.

While an expression is being _executed_, objects, temporary values,
and function return addresses are pushed onto an internal QSL stack.
The constant `MAX_STACK_DEPTH` determines how much space is
*permanently* allocated to this stack.

The suggestion is to run (some approximation of) your application under
the Linux version of QSL with generous sizes (the ones provided by
default should be adequate), then use the values returned by the
`(mem)` function to estimate how far these can be reduced for the
Arduino version.
