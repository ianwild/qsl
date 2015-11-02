#ifndef QSL_SYMBOLS_H
#define QSL_SYMBOLS_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj find_symbol        (uint8_t *spelling, uint16_t len);
obj symbol_value       (obj sym);
obj set_symbol_value   (obj sym, obj val);

END_EXTERN_C

#endif /* QSL_SYMBOLS_H */
