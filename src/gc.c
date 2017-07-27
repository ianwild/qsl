#include "announce.h"
#include "dbg.h"
#include "gc.h"
#include "eval.h"
#include "io.h"
#include "obj.h"
#include "rom-symbols.h"
#include "stack.h"

static obj next_to_sweep;


void want_obj (obj o)
{
  if (o <= LAST_ROM_OBJ || o > last_allocated_object)
    return;
  objhdr *p = get_header (o);
  if (o < next_to_sweep && (GET_FLAGS (p) & gc_wanted) == 0)
    next_to_sweep = o;
  SET_FLAGS (p, gc_wanted);
}

static void mark_roots (void)
{
  for (obj i = LAST_ROM_OBJ + 1; i <= last_allocated_object; i += 1)
  {
    objhdr *p = get_header (i);
    if ((GET_FLAGS (p) & gc_fixed) ||
        (GET_TYPE (p) == symbol_type && p -> u.symbol_val.global_fn) ||
        (GET_TYPE (p) == global_binding_type) ||
        (GET_TYPE (p) == closure_type &&
         p -> u.closure_val.environment == obj_T))
      SET_FLAGS (p, gc_wanted);
  }
}

void do_gc (void)
{
  next_to_sweep = LAST_ROM_OBJ + 1;
  announce (ann_gc_starting);
  mark_roots ();

  while (next_to_sweep <= last_allocated_object)
  {
    objhdr *p = get_header (next_to_sweep++);
    if ((GET_FLAGS (p) & (gc_wanted | gc_scanned)) == gc_wanted)
    {
      SET_FLAGS (p, gc_scanned);
      switch (GET_TYPE (p))
      {
      case closure_type:
        want_obj (p -> u.closure_val.environment);
        want_obj (p -> u.closure_val.lambda_obj);
        break;

      case lambda_type:
        want_obj (p -> u.lambda_body.opcodes);
        want_obj (p -> u.lambda_body.constants);
        break;

      case symbol_type:
        want_obj (p -> u.symbol_val.global_fn);
        break;

      case cons_type:
      case global_binding_type:
        want_obj (p -> u.cons_val.car_cell);
        want_obj (p -> u.cons_val.cdr_cell);
        break;

      case array_type:
      case environment_type:
      {
        obj *q = p -> u.array_val;
        uint16_t len = *q++;
        while (len--)
          want_obj (*q++);
        break;
      }
      }
    }
  }

  compact_string_space ();

  obj high_water_mark = LAST_ROM_OBJ;

  for (obj i = LAST_ROM_OBJ + 1; i <= last_allocated_object; i += 1)
  {
    objhdr *p = get_header (i);
    if (GET_FLAGS (p) & gc_wanted)
    {
      CLR_FLAGS (p, gc_wanted | gc_scanned);
      high_water_mark = i;
    }
    else
      p -> control = unallocated_type;
  }

  last_allocated_object = high_water_mark;
  announce (ann_gc_finished);
}
