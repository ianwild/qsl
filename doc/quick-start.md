QSL Quick Start
===============

Preliminaries
-------------

I know this works with Linux, and shouldn't be too far away from
working with most other Unix-a-likes.  It might even work with
Windows, especially if you've got Cygwin or MSYS installed, but I've
got no real interest in that direction.

I assume you've got a few tools on your computer (at least `make` and
`awk`), and that you've installed and tested the Arduino IDE (for the
Arduino libraries, the `ttyUSB` permissions, and the `avr-gcc`
tool-chain).

Building QSL as an Arduino Library
----------------------------------

Get a copy of the QSL sources - either download the ZIP file or clone
the repo.

Go into the `src` directory and type `make lib`.

With the IDE _not_ running, move the (newly created) QSL directory to
your Arduino `libraries` directory.

Testing the New Library
-----------------------

Start the IDE and create a new sketch containing:

    #include <QSL.h>

    void setup() {
    }

    void loop() {
      QSL::repl ();
    }

Upload the sketch to the Arduino.

Open the Serial Monitor (`Ctrl-Shift-M`), enter `(+ 3 4)`, and click
`Send`.

Let's Try Blink
---------------

Copy and paste this into the Serial Monitor:

    (let ((state t))
      (defun blink ()
        (pin 13 state)
        (setq state (not state))))

    (on-tick 500 blink)

    (on-serial
     (lambda ()
       (cond
        ((> (readchar) ? ) (setq running nil)))))

    (setq running t)
    (while running
      (wait-for-event)
      (apply (next-event)))

and click `Send`.  The LED should start flashing.

Send `.` to stop.
