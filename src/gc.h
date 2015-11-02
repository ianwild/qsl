#ifndef QSL_GC_H
#define QSL_GC_H

#include "obj.h"
#include "target.h"

START_EXTERN_C

void do_gc (void);

extern obj working_root;

#if TARGET_ARDUINO
extern obj tick_action, serial_action;
#endif

END_EXTERN_C

#endif /* QSL_GC_H */
