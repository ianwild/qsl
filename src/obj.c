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

#if USE_STDIO
static objhdr header_table [2000];
#else
static objhdr header_table [200];
#endif
static uint8_t *string_space_top;
static uint8_t *string_space_bottom;

obj last_allocated_object = FIRST_RAM_OBJ - 1;

#include "rom-symbols.ci"

void init_memory (void)
{
  string_space_bottom = string_space_top =
    (uint8_t *) header_table + sizeof (header_table);
}

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

static void check_available_space (int16_t string_space_needed)
{
  bool once = false;
  for (;;)
  {
    objhdr *p = header_table + last_allocated_object - FIRST_RAM_OBJ;
    uint8_t *new_bottom = string_space_bottom - string_space_needed;
    if ((uint8_t *)(p + 1) < new_bottom)
      return;
    if (once)
      throw_error (no_mem);
    once = true;
    do_gc ();
  }
}

obj new_object (enum typecode type, objhdr **hdr)
{
  check_available_space (0);
  obj res = (last_allocated_object += 1);
  objhdr *p = header_table + res - FIRST_RAM_OBJ;
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

  check_available_space (true_size);
  obj res = (last_allocated_object += 1);
  objhdr *p = header_table + res - FIRST_RAM_OBJ;
  memset (p, 0, sizeof (*p));
  string_space_bottom -= sizeof (obj);
  *(obj *) string_space_bottom = res;
  switch (p -> xtype = type)
  {
  case string_type:
    *--string_space_bottom = size;
    p -> u.string_val = string_space_bottom;
    string_space_bottom -= size;
    break;
  case symbol_type:
    *--string_space_bottom = size;
    p -> u.symbol_val.spelling = string_space_bottom;
    string_space_bottom -= size;
    break;
  case array_type:
  case environment_type:
    string_space_bottom -= sizeof (uint16_t);
    *(uint16_t*) string_space_bottom = size;
    p -> u.array_val = (obj *) string_space_bottom;
    string_space_bottom -= size * sizeof (obj);
    memset (string_space_bottom, 0, size * sizeof (obj));
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
  if (o < FIRST_RAM_OBJ)
    return (rom_symbol_type);
  return (header_table [o - FIRST_RAM_OBJ].xtype);
}
