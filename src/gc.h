#ifndef QSL_GC_H
#define QSL_GC_H

#include "obj.h"

void do_gc (void);

extern obj working_root;

#if ! USE_LINUX
extern obj tick_action, serial_action;
#endif

#endif /* QSL_GC_H */
