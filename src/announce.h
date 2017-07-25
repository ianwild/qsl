#ifndef QSL_ANNOUNCE_H
#define QSL_ANNOUNCE_H

#include "target.h"

START_EXTERN_C

enum __attribute__ ((packed)) announcement
{
  ann_startup,
  ann_clear_memory,
  ann_computation_aborted,
  ann_gc_starting,

  ann_gc_finished
};


void   announce                (enum announcement ann);

void   compiler_announce       (enum announcement ann);
void   embed_announce          (enum announcement ann);
void   eval_announce           (enum announcement ann);
void   io_announce             (enum announcement ann);
void   obj_announce            (enum announcement ann);
void   stack_announce          (enum announcement ann);

END_EXTERN_C


#endif // QSL_ANNOUNCE_H
