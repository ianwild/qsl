#ifndef QSL_EVAL_H
#define QSL_EVAL_H

#include "types.h"

obj fn_eval       (obj args);
obj fn_apply      (obj args);

obj eval_internal (obj expr);

extern obj current_environment;

#endif /* QSL_EVAL_H */
