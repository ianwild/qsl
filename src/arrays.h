#ifndef QSL_ARRAYS_H
#define QSL_ARRAYS_H

#include "target.h"
#include "types.h"

START_HEADER_FILE

obj   fn_make_string    (uint8_t *argc);
obj   fn_make_array     (uint8_t *argc);
obj   fn_length         (uint8_t *argc);
obj   fn_aref           (uint8_t *argc);
obj   fn_aset           (uint8_t *argc);
obj   fn_char_code      (uint8_t *argc);
obj   fn_code_char      (uint8_t *argc);

END_HEADER_FILE

#endif // QSL_ARRAYS_H
