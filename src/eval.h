#ifndef QSL_EVAL_H
#define QSL_EVAL_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj    fn_apply             (uint8_t *argc);

obj    interpret_top_level  (obj closure);

extern obj current_environment;

END_EXTERN_C

#endif // QSL_EVAL_H
