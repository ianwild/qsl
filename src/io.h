#ifndef QSL_IO_H
#define QSL_IO_H

#include "target.h"
#include "types.h"

START_HEADER_FILE

void     printc           (uint8_t ch);
void     print1           (obj o);
obj      internal_read    (void);
void     print_int        (int32_t n0);
void     print_prompt     (void);
void     print_result     (obj o);

obj      fn_peekchar      (uint8_t *argc);
obj      fn_readchar      (uint8_t *argc);
obj      fn_read          (uint8_t *argc);
obj      fn_print         (uint8_t *argc);

END_HEADER_FILE

#endif // QSL_IO_H
