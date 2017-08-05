#ifndef QSL_STACK_H
#define QSL_STACK_H

#include "target.h"
#include "types.h"

START_EXTERN_C

uint8_t    get_stack_depth       (void);
uint8_t    get_stack_deepest     (void);
void       stack_push            (obj o);
void       stack_pop             (uint8_t n);
obj        get_arg               (uint8_t idx);
obj        snip_arg              (uint8_t idx);
uint16_t   get_and_incr_arg      (uint8_t idx);
obj        pop_arg               (void);
void       adjust_argc           (uint8_t *argc, uint8_t wanted);

void       stack_reinit          (void);

#if WITH_TRACE
void       print_stack_depth     (void);
void       dump_stack            (void);
#else
#define print_stack_depth()
#define dump_stack()
#endif

END_EXTERN_C

#endif // QSL_STACK_H
