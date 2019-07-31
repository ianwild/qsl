#include <string.h>

#include "announce.h"
#include "buffer-limits.h"
#include "dbg.h"
#include "gc.h"
#include "obj.h"
#include "stack.h"

START_IMPLEMENTATION

static_assert (MAX_STACK_DEPTH > 8, "stack unreasonably small");

#if MAX_STACK_DEPTH < 256
  typedef uint8_t stack_depth_t;
#else
  typedef uint16_t stack_depth_t;
#endif

static obj stack_obj;
static obj *base;
static stack_depth_t depth;
static stack_depth_t deepest;

#if WITH_TRACE
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

uint16_t get_stack_depth (void)
{
  return (depth);
}

uint16_t get_stack_deepest (void)
{
  return (deepest);
}


void stack_push (obj o)
{
  if (depth == MAX_STACK_DEPTH)
    throw_error (no_mem);
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

void stack_reinit (void)
{
  if (! stack_obj)
    stack_obj = new_extended_object (array_type, MAX_STACK_DEPTH);
  base = get_header (stack_obj) -> u.array_val + 1;
}

void stack_announce (enum announcement ann)
{
  switch (ann)
  {
  case ann_gc_starting:
    if (base)
      for (stack_depth_t n = depth; n < MAX_STACK_DEPTH; n += 1)
        base [n] = obj_NIL;
    want_obj (stack_obj);
    base = NULL;
    break;

  case ann_computation_aborted:
    depth = 0;
    break;

  case ann_startup:
  case ann_gc_finished:
    stack_reinit ();
    break;

  case ann_shutting_down:
    depth = 0;
    base = NULL;
    stack_obj = obj_NIL;
    break;

  default:
    break;
  }
}

END_IMPLEMENTATION
