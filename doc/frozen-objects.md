Frozen Objects
==============

Theory of Operation
-------------------

There are two sets of "object tables" in QSL - one in ROM, the other
in RAM.  However, ROM is plentiful, whereas RAM is scarce, so it would
be a good idea to somehow move tables of (necessarily constant) objects
from RAM to ROM.  The only problem is that the ROM tables are created
by the C compiler, statically, under Linux, but the RAM contents are
defined in Lisp, dynamically, on an Arduino, so we need to somehow do
at compile time things normally delayed until run time.

However, ...

There are two builds of QSL - the Linux version and the Arduino version.
Originally, the Linux version was intended only for quick testing.
Because it's supposed to be for testing, the Linux memory layout is
pretty similar to the Arduino one, so if we can make a special Linux
build that dumps out its RAM as C source code, ...

Building Frozen Objects
-----------------------

Edit `frozen.lisp` to define whatever you'd like to have built in
to the next version.

Now enter:

      $ make clean
      $ make frozen

to create four files, `bytes.ci`, `words.ci`, `objects.ci`, and a new
version of `frozen-objects.h`.  These contain the internal representation
of whatever `frozen.lisp` defines, in a form the C compiler can
understand.

Now a `make` (or `make arduino`) will create a version with _three_  sets
of object tables: the ROM built-ins, the RAM working storage and, between
the two, the "frozen" tables copied from (Linux) RAM to (Arduino) ROM.

