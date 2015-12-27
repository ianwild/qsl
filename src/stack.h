#ifndef QSL_STACK_H
#define QSL_STACK_H

/*
  Instead of passing a newly-consed array on each function call,
  the bytecode version will have an implicit stack, and simply
  pass the number of arguments the called function should consume.

  These are the accessor functions for the stack.
*/

void  stack_push   (obj o);
void  stack_pop    (uint8_t n);
obj   get_arg      (uint8_t idx);

#endif /* QSL_STACK_H */
