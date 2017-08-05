#ifndef QSL_H
#define QSL_H

#include <stdint.h>

namespace QSL {

  // call this to run QSL (note: this function never returns)

  void       repl             (void);



  // you can override these to change how QSL does I/O

  uint8_t    serial_readc     (void);
                                // wait for and return the next input char

  int16_t    serial_peekc     (void);
                                // if a char is ready, read it, else -1

  void       serial_printc    (uint8_t ch);
                                // print the character given
}

#endif // QSL_H
