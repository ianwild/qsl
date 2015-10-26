#ifndef QSL_IO_H
#define QSL_IO_H

#include "types.h"

uint8_t  readc           (void);
void     printc          (uint8_t ch);

obj      fn_read         (obj args);
obj      fn_print        (obj args);

#endif /* QSL_IO_H */
