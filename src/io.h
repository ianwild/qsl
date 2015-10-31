#ifndef QSL_IO_H
#define QSL_IO_H

#include "types.h"

int16_t  peekc           (void);
uint8_t  readc           (void);
void     pushbackc       (uint8_t ch);
void     printc          (uint8_t ch);
void     print1          (obj o);
obj      internal_read   (void);

extern bool slow_output;

obj      fn_peekchar     (obj args);
obj      fn_readchar     (obj args);
obj      fn_read         (obj args);
obj      fn_print        (obj args);

#endif /* QSL_IO_H */
