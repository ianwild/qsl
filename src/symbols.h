#ifndef QSL_SYMBOLS_H
#define QSL_SYMBOLS_H

#include "target.h"
#include "types.h"

START_HEADER_FILE

obj  find_symbol        (uint8_t *spelling, uint16_t len);
obj  symbol_value       (obj sym, bool *is_frozen);
obj  set_symbol_value   (obj sym, obj val);

END_HEADER_FILE

#endif // QSL_SYMBOLS_H
