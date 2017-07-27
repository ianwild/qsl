# QSL quick start

0)  I know this works with Linux, and shouldn't be too far away from
    working with most other Unix-a-likes.  It might even work with
    Windows, but I've got no real interest in that direction.

    It helps if your processor can handle mis-aligned loads and
    stores.  It doesn't need to handle them _well_, just not kill the
    program.

1)  Make sure you've got `arduino-mk` installed, either by using your
    OS's package manager or by grabbing a copy from Github.

2)  Grab a copy of the sources - either the ZIP file or by cloning the
    QSL repo.

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
