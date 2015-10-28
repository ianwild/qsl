#ifndef QSL_SYMBOLS_H
#define QSL_SYMBOLS_H

#include "types.h"

obj find_symbol    (uint8_t *spelling, uint16_t len);
obj symbol_value   (obj sym, obj env);

#endif /* QSL_SYMBOLS_H */
