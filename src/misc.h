#ifndef QSL_MISC_H
#define QSL_MISC_H

#include "target.h"
#include "types.h"

START_HEADER_FILE

obj       fn_not          (uint8_t *argc);
obj       fn_eq           (uint8_t *argc);
obj       fn_neq          (uint8_t *argc);
obj       fn_lt           (uint8_t *argc);
obj       fn_le           (uint8_t *argc);
obj       fn_gt           (uint8_t *argc);
obj       fn_ge           (uint8_t *argc);
obj       fn_equals       (uint8_t *argc);
obj       fn_not_equals   (uint8_t *argc);

int       compare_strings (const uint8_t *s1, uint16_t len1, bool in_rom1,
                           const uint8_t *s2, uint16_t len2, bool in_rom2);

END_HEADER_FILE

#endif // QSL_MISC_H
