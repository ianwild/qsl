#ifndef QSL_FEXPRS_H
#define QSL_FEXPRS_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj   eval_progn       (obj o, obj res);

obj   fe_progn         (obj args);
obj   fe_cond          (obj args);
obj   fe_while         (obj args);
obj   fe_quote         (obj args);
obj   fe_setq          (obj args);
obj   fe_defun         (obj args);
obj   fe_fexpr         (obj args);
obj   fe_and           (obj args);
obj   fe_or            (obj args);
obj   fe_let           (obj args);
obj   fe_let_star      (obj args);
obj   fe_apply         (obj args);

END_EXTERN_C

#endif /* QSL_FEXPRS_H */
