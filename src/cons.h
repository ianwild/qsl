#ifndef QSL_CONS_H
#define QSL_CONS_H

#include "target.h"
#include "types.h"

START_HEADER_FILE

obj  fn_car      (uint8_t *argc);
obj  fn_cdr      (uint8_t *argc);
obj  fn_cons     (uint8_t *argc);
obj  fn_list     (uint8_t *argc);
obj  fn_rplaca   (uint8_t *argc);
obj  fn_rplacd   (uint8_t *argc);

obj      cons           (obj car, obj cdr);
void     decons         (obj cons, obj *car, obj *cdr);
uint16_t internal_len   (obj o);

END_HEADER_FILE

#endif // QSL_CONS_H
