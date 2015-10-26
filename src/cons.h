#ifndef QSL_CONS_H
#define QSL_CONS_H

#include "types.h"

obj  fn_car    (obj args);
obj  fn_cdr    (obj args);
obj  fn_cons   (obj args);
obj  fn_list   (obj args);
obj  fn_rplca  (obj args);
obj  fn_rplcd  (obj args);

obj      cons           (obj car, obj cdr);
void     decons         (obj cons, obj *car, obj *cdr);
uint16_t internal_len   (obj o);

#endif /* QSL_CONS_H */
