#include "obj.h"

static obj next_to_sweep;

static void want_obj (obj o)
{
  if (o > LAST_POSSIBLE_OBJECT)
    return;
  objhdr *p = get_header (o);
  if (o < next_to_sweep && (p -> flags & gc_wanted) == 0)
    next_to_sweep = o;
  p -> flags |= gc_wanted;
}

static void mark_roots (void)
{
}

static void compact_string_space (void)
{
}

void do_gc (void)
{
  next_to_sweep = obj_NIL;

  mark_roots ();

  while (next_to_sweep <= last_allocated_object)
  {
    objhdr *p = get_header (next_to_sweep++);
    if (p -> flags & gc_fixed)
      p -> flags |= gc_wanted;
    if ((p -> flags & (gc_wanted | gc_swept)) == gc_wanted)
    {
      p -> flags |= gc_swept;
      switch (p -> type)
      {
      case closure_type:
        want_obj (p -> u.closure_val.environment);
        want_obj (p -> u.closure_val.code);
        break;

      case symbol_type:
        want_obj (p -> u.symbol_val.global_fn);
        break;

      case cons_type:
        want_obj (p -> u.cons_val.car_cell);
        want_obj (p -> u.cons_val.cdr_cell);
        break;
      }
    }
  }

  compact_string_space ();

  obj i = obj_NIL;
  obj high_water_mark = obj_NIL;

  while (i <= last_allocated_object)
  {
    objhdr *p = get_header (i);
    if (p -> flags & gc_wanted)
    {
      p -> flags &= ~ (gc_wanted | gc_swept);
      high_water_mark = i;
    }
    else
      p -> type = unallocated_type;
  }

  last_allocated_object = high_water_mark;
}


