#ifndef QSL_IO_H
#define QSL_IO_H

#include "types.h"

uint8_t  readc           (void);
void     printc          (uint8_t ch);
#if ! USE_STDIO
void     error_helper    (char *file, int line, char *msg);
#endif
void msg (char *txt);

obj      fn_read         (obj args);
obj      fn_print        (obj args);

#endif /* QSL_IO_H */
