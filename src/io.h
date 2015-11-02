#ifndef QSL_IO_H
#define QSL_IO_H

#include "target.h"
#include "types.h"

START_EXTERN_C

int16_t  peekc            (void);
uint8_t  readc            (void);
void     pushbackc        (uint8_t ch);
void     printc           (uint8_t ch);
void     print1           (obj o);
obj      internal_read    (void);
void     print_rom_string (const char *p);
void     print_int        (int32_t n0);

extern bool slow_output;

obj      fn_peekchar     (obj args);
obj      fn_readchar     (obj args);
obj      fn_read         (obj args);
obj      fn_print        (obj args);

END_EXTERN_C

#endif /* QSL_IO_H */
