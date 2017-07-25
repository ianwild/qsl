#ifndef QSL_GC_H
#define QSL_GC_H

#include "target.h"
#include "types.h"

START_EXTERN_C

void     do_gc          (void);
void     want_obj       (obj x);

END_EXTERN_C

#endif // QSL_GC_H
