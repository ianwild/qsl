#ifndef QSL_GC_HOOKS_H
#define QSL_GC_HOOKS_H

#include "target.h"
#include "types.h"


START_EXTERN_C

void     want_obj              (obj x);
void     free_compile_buffers  (void);
void     free_io_buffers       (void);
void     mark_stack            (void);

#if TARGET_ARDUINO
void     embed_mark_roots      (void);
#endif

extern obj working_root;

END_EXTERN_C

#endif // QSL_GC_HOOKS_H
