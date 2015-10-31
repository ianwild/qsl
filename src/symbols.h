#ifndef QSL_SYMBOLS_H
#define QSL_SYMBOLS_H

#include "types.h"

obj find_symbol        (uint8_t *spelling, uint16_t len);
obj symbol_value       (obj sym);
obj set_symbol_value   (obj sym, obj val);

#endif /* QSL_SYMBOLS_H */
