#include "obj.h"
#include "stack.h"

static obj xyzzy [1024];
static obj *stack = xyzzy;

uint16_t get_stack_depth (void)
{
  return (stack - xyzzy);
}

void stack_push (obj o)
{
  *stack++ = o;
}

void stack_pop (uint8_t n)
{
  stack -= n;
}

obj get_arg (uint8_t idx)
{
  return (* (stack - idx - 1));
}

uint16_t get_and_incr_arg (uint8_t idx)
{
  obj *addr = stack - idx - 1;
  obj res = *addr;
  *addr = res + 1;
  return (res - obj_ZERO);
}

obj pop_arg (void)
{
  return (*--stack);
}

void adjust_argc (uint8_t *argc, uint8_t wanted)
{
  uint8_t n = *argc;
  if (n > wanted)
    stack_pop (n - wanted);
  else
    while (n < wanted)
    {
      stack_push (obj_NIL);
      n += 1;
    }
  *argc = n;
}
