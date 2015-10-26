#ifndef QSL_IO_H
#define QSL_IO_H

#include "types.h"

uint8_t  readc           (void);
void     printc          (uint8_t ch);
#if ! USE_STDIO
void     error_helper    (char *file, int line, char *msg);
#endif

obj      fn_read         (obj *argv);
obj      fn_print        (obj *argv);

#endif /* QSL_IO_H */
