#include "stack.h"

static obj *stack;

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
  return (* (stack - idx));
}

obj pop_arg (void)
{
  return (*--stack);
}

