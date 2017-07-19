#include <string.h>

#include "arrays.h"
#include "cons.h"
#include "dbg.h"
#include "eval.h"
#include "fexprs.h"
#include "gc.h"
#include "hardware.h"
#include "integer.h"
#include "misc.h"
#include "io.h"
#include "obj.h"
#include "rom-symbols.h"
#include "target.h"
#include "types.h"

static const char PROGMEM this_file [] = __FILE__;

#if TARGET_ARDUINO
  #define TOTAL_SIZE 1280
#else
  #define TOTAL_SIZE 12800
#endif

static uint8_t string_space [TOTAL_SIZE];
static uint8_t *string_space_top = string_space;
static objhdr *const headers =
  (objhdr *) (string_space + sizeof (string_space));

obj last_allocated_object = LAST_ROM_OBJ;

/*
  Working memory looks like this:

     headers                            -> ---------  high memory
                                           |       |
     get_header(last_allocated_object)  -> |~ v v ~|
                                           |       |
                                           |       |  (gap)
                                           |       |
     string_space_top                   -> |~ ^ ^ ~|
                                           |       |
     string_space                       -> ---------  low memory

  Every real object has a header in the `headers` array.  The
  `headers` array is negatively indexed, and growns downwards through
  memory.  Extended objects (strings, arrays, and their close
  relatives) also have a "body" in the low-memory section, which grows
  upwards.  Free memory is the gap between the two.

  The garbage collector can compact the string_space area, but doesn't
  move the `headers` contents, except that unused elements might get
  disappeared.

  The free memory area can be used as temporary storage by the `read`
  and `compile` primitives.  Alternatively, adding a `resize_object()`
  function would allow these temporary storage areas to live in the
  `string_space`.  (I think this might be how the stack gets
  implemented.)

*/

#include "rom-symbols.ci"

void memstats (void)
{
  uint8_t *ht = (uint8_t*) headers;
  uint8_t *hb = (uint8_t *) get_header (last_allocated_object);
  printc ('<');
  print_int (string_space_top - string_space);
  printc ('|');
  print_int (hb - string_space_top);
  printc ('|');
  print_int (ht - hb);
  printc ('>');
}

static objhdr *GET_HEADER (obj o)
{
  // just like get_header(), but with less checking
  return (headers - (o - LAST_ROM_OBJ));
}

objhdr *get_header (obj o)
{
  if (o <= LAST_ROM_OBJ || o > last_allocated_object)
    throw_error (bad_obj);
  return (GET_HEADER (o));
}

const rom_object *get_rom_header (obj o)
{
  if (o > LAST_ROM_OBJ)
    throw_error (bad_obj);

  return (rom_symbols + o);
}

uint8_t *get_spelling (obj o, uint16_t *len)
{
  objhdr *hdr = get_header (o);
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
  if (len)
    *len = *p;
  return (p + 1);
}

const uint8_t *get_rom_spelling (obj o, uint16_t *len)
{
  if (o > LAST_ROM_OBJ)
    throw_error (bad_obj);
  const uint8_t *p = (void *)pgm_read_word_near (&rom_symbols [o].name);
  *len = pgm_read_byte_near (p);
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
    if (string_space_top + really_needed < (uint8_t *) GET_HEADER (new_last))
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
  objhdr *p = GET_HEADER (res);
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
  objhdr *p = GET_HEADER (res);
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
    *(obj *) string_space_top = (obj) size;
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
  if (o <= LAST_ROM_OBJ)
    return (rom_symbol_type);
  if (o > last_allocated_object)
    return (unallocated_type);
  return (GET_HEADER (o) -> xtype);
}

void compact_string_space (void)
{
  uint8_t *from = string_space;
  uint8_t *to = string_space;

  while (from < string_space_top)
  {
    obj o = *(obj *)from;
    objhdr *p = get_header (o);
    uint16_t len = 0;
    void *back_ptr = NULL;
    switch (p -> xtype)
    {
    case symbol_type:
    case string_type:
      len = * (from + sizeof (obj)) + 1;
      back_ptr = p -> u.string_val;
      break;
    case array_type:
    case environment_type:
      len = * (obj *) (from + sizeof (obj)) + 1;
      len *= sizeof (obj);
      back_ptr = p -> u.array_val;
      break;
    }
    len += sizeof (obj);
    if ((p -> flags & gc_wanted) && (back_ptr == from + sizeof (obj)))
    {
      if (to != from)
        memmove (to, from, len);
      p -> u.string_val = to + sizeof (obj);
      to += len;
    }
    from += len;
  }

  string_space_top = to;
}
