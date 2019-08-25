Options to `make`
=================

`make qsl`
----------

(or just `make`)

This builds a Linux executable, from the same sources as the Arduino
version, but without the `hardware.cpp` functions.  You can use it to
debug any extra functions you've added to the rest of the interpreter.
(It's often useful to `make simple` first, depending on what you're
testing.)


`make simple`
-------------

Empties the "frozen objects" header, thus disabling the related functions
in the _next_ build (either `make` or `make arduino`).


`make frozen`
-------------

Re-compiles QSL to include a special "...and then dump RAM" routine,
then runs this new version with input from `frozen.lisp`.  The end result
is that anything defined in `frozen.lisp` becomes defined in the _next_
build (either `make` or `make arduino`).


`make arduino`
--------------

*Links* (note: _not_ "copies") all the `*.c*` and `*.h*` files into a
newly created directory, `arduino`, renames the C files to `*.cpp`,
except `main.c` which becomes `main.ino`, then attempts to build the
interpreter using the `arduino-mk` mechanism.

Since the files are linked, you can make corrections in either
directory.


`make upload`
-------------

Does a `make arduino`, as above, then tries to used `arduino-mk` to
upload to the target platform.


`make lib`
----------

(Note: this hasn't been tested with frozen objects.)

*Copies* (note: _not_ "links") all the the `*.c*` and `*.h*` files
into a newly created directory, `QSL/lib`, renames the C files to
`*.cpp`, adds a `qsl-options.h` file that contains the various `WITH_`
options extracted from the `arduino.mak` file, and adds a
`library.properties` file that marks the whole `QSL` directory as an
Arduino library.

If you move the `QSL` directory to your `sketchbook/libraries`
directory, then `QSL` will be available from the UI library manager.
This _should_ work with any UI version later than 1.5, but it's only
been tested with 1.8.3.

(The `qsl-options.h` file is necessary because there seems to be no
way to pass command-line defines through the Arduino UI.)


And, for completeness,


`make clean`
------------

Removes accumulated crud.


`make tar`
----------

Wraps the whole thing into a TGZ file.


`make tags`
-----------

Builds either a `vi` or an `emacs` cross-referencing file, depending on
which of `ctags` or `etags` it finds first.


`make qsl-b`
------------

For internal use only.
