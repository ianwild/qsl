#ifndef QSL_IO_H
#define QSL_IO_H

#include "types.h"

int      peekc           (void);
uint8_t  readc           (void);
void     printc          (uint8_t ch);

obj      fn_peekchar     (obj args);
obj      fn_readchar     (obj args);
obj      fn_read         (obj args);
obj      fn_print        (obj args);

#endif /* QSL_IO_H */
