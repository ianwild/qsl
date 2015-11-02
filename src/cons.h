#ifndef QSL_CONS_H
#define QSL_CONS_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj  fn_car      (obj args);
obj  fn_cdr      (obj args);
obj  fn_cons     (obj args);
obj  fn_list     (obj args);
obj  fn_rplaca   (obj args);
obj  fn_rplacd   (obj args);

obj      cons           (obj car, obj cdr);
void     decons         (obj cons, obj *car, obj *cdr);
uint16_t internal_len   (obj o);

END_EXTERN_C

#endif /* QSL_CONS_H */
