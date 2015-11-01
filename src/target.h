#ifndef QSL_TARGET_H
#define QSL_TARGET_H

#if TARGET_ARDUINO

#include <Arduino.h>

#else

/*

  To save RAM we need to put compile-time tables into flash memory
  whenever possible.  Of course, if we're running on Linux we don't
  need this, so get rid of the compiler attribute:

*/

#define PROGMEM

/*

  But once the tables are in flash, the ATmega's Harvard architecture
  means we need special code to get things back.  The Linux version
  can just use *(x).

*/

#define pgm_read_byte_near(x) (*(x))
#define pgm_read_word_near(x) (*(x))


#endif

#endif /* QSL_TARGET_H */