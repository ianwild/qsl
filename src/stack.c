#include "stack.h"

static obj xyzzy [1024];
static obj *stack = xyzzy;

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

obj pop_arg (void)
{
  return (*--stack);
}

