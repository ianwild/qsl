# QSL quick start

0)  I know this works with Linux, and shouldn't be too far away from
    working with most other Unix-a-likes.  It might even work with
    Windows, but I've got no real interest in that direction.

    It helps if your processor can handle mis-aligned loads and
    stores.  It doesn't need to handle them _well_, just not kill the
    program.  (There's a `static_assert()` in `compiler.c` that
    tries to check this for you.)
    
    I assume you've got tools installed to build an executable
    for your computer (at least `gcc`, `make`, and `awk`), and
    that you've installed and tested the Arduino IDE (for the
    Arduino libraries, the `ttyUSB` permissions,  and the `avr-gcc`
    tool-chain).

1)  You'll also need `arduino-mk` installed, either by using your
    OS's package manager or by grabbing a copy from Github.

2)  Get a copy of the QSL sources - either download the ZIP file or
    clone the repo.

3)  To build the test version, go into the `src` directory and type
    `make`. If this works, type something like:

        echo '(+ 3 4)' | ./qsl

    and see if the results look reasonable.

4)  Edit `arduino.mak` to make sure the `BOARD_TAG` and `MONITOR_PORT`
    variables are correct, and that the last line points to your
    `arduino-mk` installation.  (See your `arduino-mk` documentation
    for `BOARD_TAG` and `MONITOR_PORT`.)

5)  To build the Arduino version, type `make arduino`.  This will
    create an `arduino` sub-directory, link all the source files, then
    do a build.  If that works, ...

6)  To upload to the target hardware, type `make upload`.

7)  To test it, you can try `make -C arduino monitor`.  This "works",
    but since QSL doesn't echo what you type, it's a bit unpleasant.
    Instead, I prefer:

        picocom /dev/ttyUSB0 9600 --emap crcrlf -c --imap lfcrlf

    to get local echo and decent CRLF handling.  Either way, at the
    `qsl>` prompt, try `(+ 3 4)` and see what happens.
