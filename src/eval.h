#ifndef QSL_EVAL_H
#define QSL_EVAL_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj  fn_eval           (obj args);

obj  eval_internal     (obj expr);
obj  apply_internal    (obj fn, obj args);

extern obj current_environment;

END_EXTERN_C

#endif /* QSL_EVAL_H */
