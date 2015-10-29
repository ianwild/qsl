#include <stdio.h>
#include "gc.h"
#include "eval.h"
#include "obj.h"
#include "rom-symbols.h"

static obj next_to_sweep;
obj working_root;

static void want_obj (obj o)
{
  if (o <= LAST_ROM_OBJ || o > last_allocated_object)
    return;
  objhdr *p = get_header (o);
  if (o < next_to_sweep && (p -> flags & gc_wanted) == 0)
    next_to_sweep = o;
  p -> flags |= gc_wanted;
}

static void mark_roots (void)
{
  obj i;
  for (i = LAST_ROM_OBJ + 1; i <= last_allocated_object; i += 1)
  {
    objhdr *p = get_header (i);
    if ((p -> flags & gc_fixed) || (p -> xtype == global_binding_type))
      p -> flags |= gc_wanted;
  }
}

void do_gc (void)
{
  next_to_sweep = LAST_ROM_OBJ + 1;

  mark_roots ();
  want_obj (working_root);
  want_obj (current_environment);

  while (next_to_sweep <= last_allocated_object)
  {
    objhdr *p = get_header (next_to_sweep++);
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
      case global_binding_type:
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

  obj i;
  obj high_water_mark = obj_NIL;

  for (i = LAST_ROM_OBJ + 1; i <= last_allocated_object; i += 1)
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

