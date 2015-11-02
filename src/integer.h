#ifndef QSL_INTEGER_H
#define QSL_INTEGER_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj       create_int      (int32_t val);
int32_t   get_int_val     (obj o);

obj       fn_plus         (obj args);
obj       fn_times        (obj args);
obj       fn_minus        (obj args);
obj       fn_divide       (obj args);

END_EXTERN_C

#endif /* QSL_INTEGER_H */
