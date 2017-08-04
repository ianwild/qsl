#ifndef QSL_H
#define QSL_H

#include <stdint.h>

namespace QSL {
  // call this to run QSL (note: this function never returns)
  void       repl             (void);

  // you can override these to change how QSL does I/O
  uint8_t    serial_readc     (void);
  int16_t    serial_peekc     (void);
  void       serial_printc    (uint8_t ch);
}

#endif // QSL_H
