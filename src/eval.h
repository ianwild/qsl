#ifndef QSL_EVAL_H
#define QSL_EVAL_H

#include "target.h"
#include "types.h"

START_HEADER_FILE

obj    fn_apply             (uint8_t *argc);

obj    interpret_top_level  (obj closure);

extern obj current_environment;

END_HEADER_FILE

#endif // QSL_EVAL_H
