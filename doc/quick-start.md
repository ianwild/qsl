# QSL Quick Start

0)  I know this works with Linux, and shouldn't be too far away from
    working with most other Unix-a-likes.  It might even work with
    Windows, but I've got no real interest in that direction.

    I assume you've got tools installed to build an executable for
    your computer (at least `make` and `awk`), and that you've
    installed and tested the Arduino IDE (for the Arduino libraries,
    the `ttyUSB` permissions, and the `avr-gcc` tool-chain).

1)  Get a copy of the QSL sources - either download the ZIP file or
    clone the repo.

2)  Go into the `src` directory and type `make lib`.

3)  With the IDE _not_ running, move the (newly created) QSL directory
    to your Arduino `libraries` directory.

4)  Start the IDE and create a new sketch containing:
````
        #include <QSL.h>

        void setup() {
        }

        void loop() {
          QSL::repl ();
        }
````

5)  Upload to the Arduino.

6)  Open the Serial Monitor (`Ctrl-Shift-M`) and enter `(+ 3 4)`.

7)  Now copy and paste this into the Serial Monitor:
````
        (let ((state t))
          (defun blink ()
            (pin 13 state)
            (setq state (not state))))

        (on-tick 500 blink)

        (on-serial
         (lambda ()
           (cond
            ((>= (readchar) ? ) (setq running nil)))))

        (setq running t)
        (while running
          (wait-for-event)
          (apply (next-event)))
````

8)  Send `.` to stop.
