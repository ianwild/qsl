#ifndef QSL_IO_H
#define QSL_IO_H

#include "types.h"

uint8_t  readc    (void);
void     printc   (uint8_t ch);

obj      read     (obj *argv);
obj      print    (obj *argv);

#endif /* QSL_IO_H */
