#ifndef QSL_GC_H
#define QSL_GC_H

#include "obj.h"
#include "target.h"

START_EXTERN_C

void    do_gc       (void);
void    want_obj    (obj o);

extern obj working_root;

END_EXTERN_C

#endif /* QSL_GC_H */
