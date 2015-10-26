#include <stdlib.h>
#include <string.h>

#include "gc.h"
#include "obj.h"

static objhdr *header_table;
obj last_allocated_object;

objhdr *get_header (obj o)
{
  return (header_table + o);
}

obj new_object (enum typecode type)
{
  if (last_allocated_object > 1000)
    do_gc ();
  obj res = last_allocated_object;
  last_allocated_object += 1;
  objhdr *p = header_table + res;
  memset (p, 0, sizeof (*p));
  p -> type = type;
  return (res);
}
