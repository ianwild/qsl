#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "cons.h"
#include "eval.h"
#include "fexprs.h"
#include "gc.h"
#include "integer.h"
#include "io.h"
#include "obj.h"
#include "rom-symbols.h"
#include "rom-symbols.h"
#include "types.h"

static objhdr *header_table;
obj last_allocated_object;

#include "rom-symbols.ci"

void (throw_error) (enum errcode e, char *file, int line)
{
  char *msg = "weird";
  switch (e)
  {
#define MSG(x) case x: msg = #x; break
    MSG (no_error);
    MSG (bad_type);
    MSG (bad_obj);
    MSG (bad_argc);
    MSG (div_by_zero);
#undef MSG
  }
  fprintf (stderr, "%s(%d): %s\n", file, line, msg);
  exit (1);
}

objhdr *get_header (obj o)
{
  if (o < FIRST_RAM_OBJ || o > last_allocated_object)
    throw_error (bad_obj);
  return (header_table + o - FIRST_RAM_OBJ);
}

const rom_object *get_rom_header (obj o)
{
  if (o > FIRST_RAM_OBJ)
    throw_error (bad_obj);
  return (rom_symbols + o);
}

obj new_object (enum typecode type)
{
  if (last_allocated_object > 1000)
    do_gc ();
  obj res = last_allocated_object;
  last_allocated_object += 1;
  objhdr *p = header_table + res - FIRST_RAM_OBJ;
  memset (p, 0, sizeof (*p));
  p -> xtype = type;
  return (res);
}

uint8_t get_type (obj o)
{
  if (o >= FIRST_SMALL_INT)
    return (int_type);
  if (o >= FIRST_CHAR)
    return (char_type);
  if (o > last_allocated_object)
    return (unallocated_type);
  if (o < FIRST_RAM_OBJ)
    return (rom_symbol_type);
  return (header_table [o - FIRST_RAM_OBJ].xtype);
}
