#ifndef QSL_FEXPRS_H
#define QSL_FEXPRS_H

#include "types.h"


obj  fe_progn   (obj args);
obj  fe_cond    (obj args);
obj  fe_while   (obj args);
obj  fe_quote   (obj args);
obj  fe_setq    (obj args);
obj  fe_defun   (obj args);

#endif /* QSL_FEXPRS_H */
