Tuning QSL
==========

When working with the Arduino, it is important to keep the memory
footprint as small as possible.  On the other hand, reducing memory
too much will restrict what can be done in QSL.

In `buffer_limits.h` are a number of constants that control how much
memory is allocated for the various "hidden" things QSL needs.

The suggestion is to run (some approximation of) your application under
the Linux version of QSL with generous sizes (the ones provided by
default should be adequate), then use the values returned by the
`(mem)` function to estimate how far these can be reduced for the
Arduino version.

