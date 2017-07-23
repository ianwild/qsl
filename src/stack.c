#include <string.h>

#include "buffer-limits.h"
#include "dbg.h"
#include "gc-hooks.h"
#include "obj.h"
#include "stack.h"

static_assert (MAX_STACK_DEPTH > 8, "stack unreasonably small");

static obj stack_obj;
static obj *base;
static uint8_t depth;

#if WITH_TRACE
static uint8_t deepest;

void print_stack_depth (void)
{
  TRACE (("stack depth %d/%d\n", depth, deepest));
}

void dump_stack (void)
{
#if ! TARGET_ARDUINO
  printf ("[");
  for (uint8_t i = 0; i < depth; i += 1)
    printf ("%04x, ", base [i]);
  printf ("]\n");
#endif
}
#endif

uint8_t get_stack_depth (void)
{
  return (depth);
}

void stack_push (obj o)
{
  base [depth++] = o;
  #if WITH_TRACE
  if (depth > deepest)
    deepest = depth;
  #endif
}

void stack_pop (uint8_t n)
{
  depth -= n;
}

obj get_arg (uint8_t idx)
{
  return (base [depth - idx - 1]);
}

obj snip_arg (uint8_t idx)
{
  obj *addr = &base [depth - idx - 1];
  obj res = *addr;
  memmove (addr, addr + 1, idx * sizeof (obj));
  depth -= 1;
  return (res);
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
  {
    stack_pop (n - wanted);
    n = wanted;
  }
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
  for (uint8_t n = depth; n < MAX_STACK_DEPTH; n += 1)
    base [n] = obj_NIL;
  want_obj (stack_obj);
  base = NULL;
}

void stack_reinit (void)
{
  if (! stack_obj)
    stack_obj = new_extended_object (array_type, MAX_STACK_DEPTH);
  base = get_header (stack_obj) -> u.array_val + 1;
}
