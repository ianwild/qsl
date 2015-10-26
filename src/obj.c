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

static objhdr *headers;
static uint8_t string_space [1280];
static uint8_t *string_space_top = string_space;

obj last_allocated_object = LAST_ROM_OBJ;

#include "rom-symbols.ci"

void init_memory (void)
{
  headers = (objhdr *) (string_space + sizeof (string_space));
}

objhdr *get_header (obj o)
{
  if (o <= LAST_ROM_OBJ || o > last_allocated_object)
    throw_error (bad_obj);
  return (headers - (o - LAST_ROM_OBJ));
}

const rom_object *get_rom_header (obj o)
{
  if (o > LAST_ROM_OBJ)
    throw_error (bad_obj);
  return (rom_symbols + o);
}

uint8_t *get_spelling (obj o, uint16_t *len)
{
  if (o <= LAST_ROM_OBJ || o > last_allocated_object)
    throw_error (bad_obj);
  objhdr *hdr = headers - (o - LAST_ROM_OBJ);
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
  return (p + 1);
}

const uint8_t *get_rom_spelling (obj o, uint16_t *len)
{
  if (o > LAST_ROM_OBJ)
    throw_error (bad_obj);
  const uint8_t *p = rom_symbols [o].name;
  *len = *p;
  return (p + 1);
}

static obj check_available_space (int16_t string_space_needed)
{
  bool once = false;
  for (;;)
  {
    objhdr *p = headers - 1;
    obj i = LAST_ROM_OBJ + 1;
    while (i <= last_allocated_object)
    {
      if (p -> xtype == unallocated_type)
	break;
      i += 1;
      p -= 1;
    }
    int16_t really_needed = string_space_needed;
    obj new_last = last_allocated_object;
    if (i > last_allocated_object)
    {
      really_needed += sizeof (*p);
      new_last += 1;
    }
    if (string_space_top + really_needed <
	(uint8_t *) (headers - (last_allocated_object - LAST_ROM_OBJ)))
    {
      last_allocated_object = new_last;
      return (i);
    }
    if (once)
      throw_error (no_mem);
    once = true;
    do_gc ();
  }
}

obj new_object (enum typecode type, objhdr **hdr)
{
  obj res = check_available_space (0);
  objhdr *p = headers - (res - LAST_ROM_OBJ);
  memset (p, 0, sizeof (*p));
  p -> xtype = type;
  if (hdr)
    *hdr = p;
  return (working_root = res);
}

obj new_extended_object (enum typecode type, uint16_t size)
{
  int16_t true_size = size + 1;

  switch (type)
  {
  case string_type:
  case symbol_type:
    break;
  case array_type:
  case environment_type:
    true_size *= sizeof (obj);
    break;
  default:
    throw_error (bad_type);
  }

  obj res = check_available_space (true_size);
  objhdr *p = headers - (res - LAST_ROM_OBJ);
  memset (p, 0, sizeof (*p));
  *(obj *) string_space_top = res;
  string_space_top += sizeof (obj);
  switch (p -> xtype = type)
  {
  case string_type:
    p -> u.string_val = string_space_top;
    *string_space_top = size;
    string_space_top += size + 1;
    break;
  case symbol_type:
    p -> u.symbol_val.spelling = string_space_top;
    *string_space_top = size;
    string_space_top += size + 1;
    break;
  case array_type:
  case environment_type:
    p -> u.array_val = (obj *) string_space_top;
    *(uint16_t*) string_space_top = size;
    string_space_top += sizeof (obj);
    memset (string_space_top, 0, size * sizeof (obj));
    string_space_top += size * sizeof (obj);
    break;
  }
  return (working_root = res);
}

uint8_t get_type (obj o)
{
  if (o >= FIRST_SMALL_INT)
    return (int_type);
  if (o >= FIRST_CHAR)
    return (char_type);
  if (o > last_allocated_object)
    return (unallocated_type);
  if (o <= LAST_ROM_OBJ)
    return (rom_symbol_type);
  return ((headers - (o - LAST_ROM_OBJ)) -> xtype);
}
