#ifndef QSL_STACK_H
#define QSL_STACK_H


/*
  Instead of passing a newly-consed array on each function call,
  the bytecode version will have an implicit stack, and simply
  pass the number of arguments the called function should consume.

  These are the accessor functions for the stack.
*/

#include "target.h"
#include "types.h"

START_EXTERN_C

void       stack_push            (obj o);
void       stack_pop             (uint8_t n);
obj        get_arg               (uint8_t idx);
uint16_t   get_and_incr_arg      (uint8_t idx);
obj        pop_arg               (void);

END_EXTERN_C

#endif /* QSL_STACK_H */
