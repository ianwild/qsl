#if USE_STDIO
#include <stdio.h>
#define PROGMEM
#else
#include <Arduino.h>
#endif

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

objhdr *get_header (obj o)
{
  if (o < FIRST_RAM_OBJ || o > last_allocated_object)
    throw_error (bad_obj);
  return (header_table + o - FIRST_RAM_OBJ);
}

const rom_object *get_rom_header (obj o)
{
  if (o >= FIRST_RAM_OBJ)
    throw_error (bad_obj);
  return (rom_symbols + o);
}

uint8_t *get_spelling (obj o, uint16_t *len)
{
  if (o < FIRST_RAM_OBJ || o > last_allocated_object)
    throw_error (bad_obj);
  objhdr *hdr = &header_table [o - FIRST_RAM_OBJ];
  uint8_t *p;

  switch (hdr -> xtype)
  {
  case string_type:
    p = hdr -> u.string_val;
    break;

  case symbol_type:
    p = hdr -> u.symbol_val.spelling;
    break;

  default:
    throw_error (bad_obj);
    p = NULL;
  }
  *len = *p;
  return (p - *p);
}

const uint8_t *get_rom_spelling (obj o, uint16_t *len)
{
  if (o >= FIRST_RAM_OBJ)
    throw_error (bad_obj);
  const uint8_t *p = rom_symbols [o].name;
  *len = *p;
  return (p - *p);
}


obj new_object (enum typecode type, objhdr **hdr)
{
  if (last_allocated_object > 1000)
    do_gc ();
  obj res = last_allocated_object;
  last_allocated_object += 1;
  objhdr *p = header_table + res - FIRST_RAM_OBJ;
  memset (p, 0, sizeof (*p));
  p -> xtype = type;
  if (hdr)
    *hdr = p;
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
