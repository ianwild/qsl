#ifndef QSL_CONS_H
#define QSL_CONS_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj  fn_car      (uint8_t *argc);
obj  fn_cdr      (uint8_t *argc);
obj  fn_cons     (uint8_t *argc);
obj  fn_list     (uint8_t *argc);
obj  fn_rplaca   (uint8_t *argc);
obj  fn_rplacd   (uint8_t *argc);

obj      cons           (obj car, obj cdr);
void     decons         (obj cons, obj *car, obj *cdr);
uint16_t internal_len   (obj o);

END_EXTERN_C

#endif /* QSL_CONS_H */
