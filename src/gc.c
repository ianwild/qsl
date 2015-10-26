#include "gc.h"
#include "obj.h"
#include "rom-symbols.h"

static obj next_to_sweep;
obj working_root;

static void want_obj (obj o)
{
  if (o < FIRST_RAM_OBJ || o > last_allocated_object)
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
  next_to_sweep = FIRST_RAM_OBJ;

  mark_roots ();
  want_obj (working_root);

  while (next_to_sweep <= last_allocated_object)
  {
    objhdr *p = get_header (next_to_sweep++);
    if ((p -> flags & (gc_fixed | gc_wanted)) == gc_fixed)
      p -> flags |= gc_wanted;
    if ((p -> flags & (gc_wanted | gc_scanned)) == gc_wanted)
    {
      p -> flags |= gc_scanned;
      switch (p -> xtype)
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

      case array_type:
      case environment_type:
      {
	obj *q = p -> u.array_val;
	uint16_t len = *q;
	q -= len;
	while (len--)
	  want_obj (*q++);
	break;
      }
      }
    }
  }

  compact_string_space ();

  obj i = FIRST_RAM_OBJ;
  obj high_water_mark = obj_NIL;

  while (i <= last_allocated_object)
  {
    objhdr *p = get_header (i);
    if (p -> flags & gc_wanted)
    {
      p -> flags &= ~ (gc_wanted | gc_scanned);
      high_water_mark = i;
    }
    else
      p -> xtype = unallocated_type;
  }

  last_allocated_object = high_water_mark;
}


