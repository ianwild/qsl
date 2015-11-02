#ifndef QSL_MISC_H
#define QSL_MISC_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj       fn_not          (obj args);
obj       fn_eq           (obj args);
obj       fn_neq          (obj args);
obj       fn_lt           (obj args);
obj       fn_le           (obj args);
obj       fn_gt           (obj args);
obj       fn_ge           (obj args);
obj       fn_equals       (obj args);
obj       fn_not_equals   (obj args);

END_EXTERN_C

#endif /* QSL_MISC_H */
