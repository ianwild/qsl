#ifndef QSL_SERIAL_IO_H
#define QSL_SERIAL_IO_H

#include "target.h"
#include "types.h"

#if TARGET_ARDUINO

START_EXTERN_C

extern bool slow_output;

uint8_t    serial_readc     (void);
int16_t    serial_peekc     (void);
void       serial_printc    (uint8_t ch);

END_EXTERN_C

#endif

#endif // QSL_SERIAL_IO_H
