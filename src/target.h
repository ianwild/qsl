#ifndef QSL_TARGET_H
#define QSL_TARGET_H

#if __cplusplus
  #if WITH_NAMESPACE
    #define START_EXTERN_C namespace QSL {
    #define END_EXTERN_C   }
  #else
    #define START_EXTERN_C extern "C" {
    #define END_EXTERN_C   }
  #endif
#else
  #define START_EXTERN_C
  #define END_EXTERN_C
#endif


#if TARGET_ARDUINO

  #include <Arduino.h>

#else

  #include <stdio.h>
  #include <stdlib.h>

  // To save RAM we need to put compile-time tables into flash memory
  // whenever possible.  Of course, if we're running on Linux we don't
  // need this, so get rid of the compiler attribute:

  #define PROGMEM
  #define PSTR(x) x

  // But once the tables are in flash, the ATmega's Harvard architecture
  // means we need special code to get things back.  The Linux version
  // can just use *(x):

  #define pgm_read_byte_near(x) (*(x))
  #define pgm_read_word_near(x) (*(x))

#endif // TARGET_ARDUINO

#endif // QSL_TARGET_H
