#ifndef QSL_SERIAL_IO_H
#define QSL_SERIAL_IO_H

#include "target.h"

#if TARGET_ARDUINO

  #include "types.h"

  START_HEADER_FILE

  extern bool slow_output;

  uint8_t    serial_readc     (void);
  int16_t    serial_peekc     (void);
  void       serial_printc    (uint8_t ch);

  END_HEADER_FILE

#endif // TARGET_ARDUINO

#endif // QSL_SERIAL_IO_H
