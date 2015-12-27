#ifndef QSL_INTEGER_H
#define QSL_INTEGER_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj       create_int      (int32_t val);
int32_t   get_int_val     (obj o);

obj       fn_plus         (uint8_t argc);
obj       fn_times        (uint8_t argc);
obj       fn_minus        (uint8_t argc);
obj       fn_divide       (uint8_t argc);

END_EXTERN_C

#endif /* QSL_INTEGER_H */
