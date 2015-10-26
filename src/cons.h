#ifndef QSL_CONS_H
#define QSL_CONS_H

#include "types.h"

obj fn_car   (obj *argv);
obj fn_cdr   (obj *argv);
obj fn_cons  (obj *argv);
obj fn_list  (obj *argv);
obj fn_rplca (obj *argv);
obj fn_rplcd (obj *argv);

#endif /* QSL_CONS_H */
