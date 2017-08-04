Embedding QSL in an existing Arduino program
============================================

QSL relies on the three functions in `serial_io.cpp` for its input and
output.  All of these are marked `__attribute__((weak))` to make them
easy to override if you'd like I/O to be done differently.

If you're happy to let QSL use the normal `Serial` object, you'd
typically only need to override `serial_readc()`.  This is called by
QSL when it has nothing else to do, which is your opportunity to hand
control back to the rest of your program.

You probably have a `setup()` and a `loop()` function.  The minimal
changes you'd need are:

-   Install the QSL library, and add `#include <QSL.h>` at the top of
    your sektch.

-   Leave `setup()` untouched.

-   Rename your existing `loop()` to `old_loop()`.

-   Create a new `loop` function:

        void loop () {
          QSL::repl ();
        }

-   Create a `QSL::serial_readc()` that calls your old `loop`, but
    hands control to QSL when there's a character for it to work with:

        uint8_t QSL::serial_readc (void) {
          for (;;) {
            if (Serial.available ())
              return (Serial.read ());
            old_loop ();
          }
        }

-   That's it.


How it works
------------

The QSL Read-Eval-Print Loop, `QSL::repl()`, spends almost all of its
time waiting for a character, which it does by calling
`serial_readc()`.  Your new `serial_readc()` checks if there's a
character available, passing it back to QSL if there is.  If there's
no character, it instead calls one iteration of your `old_loop()`,
which is exactly what the Arduino system did previously.

Be aware that QSL can only respond during the intervals when your
`old_loop()` hands back control.  If your program contains a
hard-coded `delay()` then _everything_ gets delayed, including the
interactive responsiveness.
