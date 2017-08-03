QSL Less Quick (but still fairly quick) Start
=============================================

Preliminaries
-------------

I know this works with Linux, and shouldn't be too far away from
working with most other Unix-a-likes.  It might even work with
Windows, especially if you've got Cygwin or MSYS installed, but I've
got no real interest in that direction.

It helps if your processor can handle mis-aligned loads and stores.
It doesn't need to handle them _well_, just not kill the program.
(There's a `static_assert()` in `compiler.c` that tries to check this
for you.)

I assume you've got tools installed to build an executable for your
computer (at least `gcc`, `make`, and `awk`), and that you've
installed and tested the Arduino IDE (for the Arduino libraries, the
`ttyUSB` permissions, and the `avr-gcc` tool-chain).

Building and Running the Test Version
-------------------------------------

Get a copy of the QSL sources - either download the ZIP file or clone
the repo.

To build the test version, go into the `src` directory and type
`make`. If this works, type something like:

    echo '(+ 3 4)' | ./qsl

and see if the results look reasonable.


Building the Arduino Version
----------------------------

You'll also need `arduino-mk` installed, either by using your OS's
package manager or by grabbing a copy from Github.

Edit `arduino.mak` to make sure the `BOARD_TAG`, `BOARD_SUB`, and
`MONITOR_PORT` variables are correct, and that the last line points to
your `arduino-mk` installation.  (See your `arduino-mk` documentation
for `BOARD_TAG`, `MONITOR_PORT`, and other variables you might need.)

Once you're happy with `arduino.mak`, type `make arduino`.  This will
create an `arduino` sub-directory, link all the source files, then do
a build.  If that works, ...


Uploading and Testing the Arduino Version
-----------------------------------------

To upload to the target hardware, type `make upload`.

To test it, you can try `make -C arduino monitor`.  This "works", but
since QSL doesn't echo what you type, it's a bit unpleasant.  Instead,
I prefer:

    picocom /dev/ttyUSB0 9600 --emap crcrlf -c --imap lfcrlf

to get local echo and decent CRLF handling, though, of course, that
assumes you've got `picocom` installed.  Either way, at the `qsl>`
prompt, try `(+ 3 4)` and see what happens.

Let's Try Blink
---------------

Copy and paste the following at the `qsl>` prompt:

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

and see if the LED flashes.

Press `.` to stop.
