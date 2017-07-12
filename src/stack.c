#include "dbg.h"
#include "gc.h"
#include "obj.h"
#include "stack.h"

#define STACK_MAX 128

static obj stack_obj;
static obj *base;
static uint8_t depth;

static uint8_t deepest;

void print_stack_depth (void)
{
  TRACE (("stack depth %d/%d\n", depth, deepest));
}

void stack_push (obj o)
{
  base [depth++] = o;
  if (depth > deepest)
    deepest = depth;
}

void stack_pop (uint8_t n)
{
  depth -= n;
}

obj get_arg (uint8_t idx)
{
  return (base [depth - idx - 1]);
}

uint16_t get_and_incr_arg (uint8_t idx)
{
  obj *addr = &base [depth - idx - 1];
  obj res = *addr;
  *addr = res + 1;
  return (res - obj_ZERO);
}

obj pop_arg (void)
{
  return (base [--depth]);
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

void mark_stack (void)
{
  for (uint8_t n = depth; n < STACK_MAX; n += 1)
    base [n] = obj_NIL;
  want_obj (stack_obj);
  base = NULL;
}

void stack_reinit (void)
{
  if (! stack_obj)
    stack_obj = new_extended_object (array_type, STACK_MAX);
  base = get_header (stack_obj) -> u.array_val + 1;
}
