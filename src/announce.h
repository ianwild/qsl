#ifndef QSL_ANNOUNCE_H
#define QSL_ANNOUNCE_H

#include "target.h"

START_HEADER_FILE

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
void   eval_announce           (enum announcement ann);
void   hardware_announce       (enum announcement ann);
void   io_announce             (enum announcement ann);
void   obj_announce            (enum announcement ann);
void   stack_announce          (enum announcement ann);

END_HEADER_FILE


#endif // QSL_ANNOUNCE_H
