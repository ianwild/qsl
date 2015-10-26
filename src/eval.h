#ifndef QSL_EVAL_H
#define QSL_EVAL_H

#include "types.h"

obj fn_eval       (obj *argv);
obj fn_apply      (obj *argv);
obj eval_internal (obj expr, obj env);

extern obj current_environment;

#endif /* QSL_EVAL_H */
