#ifndef QSL_TARGET_H
#define QSL_TARGET_H

#include "buffer-limits.h"
#include "qsl-options.h"


#if ! __cplusplus
  #define START_HEADER_FILE
  #define END_HEADER_FILE
#elif WITH_NAMESPACE
  #define START_HEADER_FILE namespace QSL {
  #define END_HEADER_FILE   }
#else
  #define START_HEADER_FILE extern "C" {
  #define END_HEADER_FILE   }
#endif

#define START_IMPLEMENTATION \
  START_HEADER_FILE \
  static const char PROGMEM __attribute__ ((unused)) this_file [] = __FILE__;

#define END_IMPLEMENTATION END_HEADER_FILE

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

  #define pgm_read_byte_near(x)  (*(x))
  #define pgm_read_word_near(x)  (*(x))
  #define pgm_read_dword_near(x) (*(x))

#endif // TARGET_ARDUINO

#ifndef USE_DIRECT_POINTERS
  #if __AVR
    // The AVR has 16-bit pointers, so there's no space to be saved by
    // indirecting through a 16-bit index
    #define USE_DIRECT_POINTERS 1
  #elif TOTAL_HEAP_SIZE > 0xFFFFu
    // The heap is too big to be indexed by a 16-bit offset
    #define USE_DIRECT_POINTERS 1
  #else
    // Save space by suppressing the replacing pointers with a 16-bit
    // offset into the heap
    #define USE_DIRECT_POINTERS 0
  #endif
#endif // USE_DIRECT_POINTERS

#endif // QSL_TARGET_H
