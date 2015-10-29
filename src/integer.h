#ifndef QSL_INTEGER_H
#define QSL_INTEGER_H

#include "types.h"

obj fn_plus         (obj args);
obj fn_times        (obj args);
obj fn_minus        (obj args);
obj fn_divide       (obj args);

obj fn_lt           (obj args);
obj fn_le           (obj args);
obj fn_gt           (obj args);
obj fn_ge           (obj args);
obj fn_equals       (obj args);
obj fn_not_equals   (obj args);

#endif /* QSL_INTEGER_H */
