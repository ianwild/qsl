#ifndef QSL_FEXPRS_H
#define QSL_FEXPRS_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj   fe_progn         (uint8_t *argc);
obj   fe_cond          (uint8_t *argc);
obj   fe_while         (uint8_t *argc);
obj   fe_quote         (uint8_t *argc);
obj   fe_setq          (uint8_t *argc);
obj   fe_defun         (uint8_t *argc);
obj   fe_lambda        (uint8_t *argc);
obj   fe_and           (uint8_t *argc);
obj   fe_or            (uint8_t *argc);
obj   fe_let           (uint8_t *argc);
obj   fe_let_star      (uint8_t *argc);

void  compile_lambda_body    (obj body);

END_EXTERN_C

#endif /* QSL_FEXPRS_H */
