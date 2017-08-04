#ifndef QSL_SERIAL_IO_H
#define QSL_SERIAL_IO_H

#include "target.h"

#if TARGET_ARDUINO

  #include "types.h"

  START_EXTERN_C

  extern bool slow_output;

  uint8_t    serial_readc     (void);
  int16_t    serial_peekc     (void);
  void       serial_printc    (uint8_t ch);

  END_EXTERN_C

#endif // TARGET_ARDUINO

#endif // QSL_SERIAL_IO_H
