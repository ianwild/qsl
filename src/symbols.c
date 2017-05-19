#include <string.h>

#include "eval.h"
#include "obj.h"
#include "rom-symbols.h"
#include "symbols.h"
#include "target.h"

static const char PROGMEM this_file [] = __FILE__;

obj find_symbol (uint8_t *spelling, uint16_t len)
{
  obj sym;
  for (sym = 0; sym <= LAST_ROM_OBJ; sym += 1)
  {
    uint16_t rom_len;
    const uint8_t *rom_spelling = get_rom_spelling (sym, &rom_len);
    if (rom_len == len)
    {
      uint8_t *new_spelling = spelling;
      while (rom_len)
      {
        if (pgm_read_byte_near (rom_spelling++) != *new_spelling++)
          break;
        rom_len -= 1;
      }
      if (rom_len == 0)
        return (sym);
    }
  }

  for (; sym <= last_allocated_object; sym += 1)
  {
    if (get_type (sym) == symbol_type)
    {
      uint16_t ram_len;
      const uint8_t *ram_spelling = get_spelling (sym, &ram_len);
      if (ram_len == len &&
          memcmp (spelling, ram_spelling, len) == 0)
        return (sym);
    }
  }

  sym = new_extended_object (symbol_type, len);
  uint16_t dummy;
  uint8_t *new_spelling = get_spelling (sym, &dummy);
  memcpy (new_spelling, spelling, len);
  return (sym);
}

static obj *find_lexical_binding (obj sym)
{
  obj env = current_environment;
  while (env != obj_NIL)
  {
    obj *p = get_header (env) -> u.array_val;
    uint16_t len = (uint16_t) *p++ - 1;
    env = *p++;                 // parent environment
    while (len)
    {
      if (*p == sym)
        return (p + 1);
      p += 2;
      len -= 2;
    }
  }
  return (NULL);
}

static objhdr *find_global_binding (obj sym)
{
  obj res;
  for (res = LAST_ROM_OBJ + 1; res <= last_allocated_object; res += 1)
    if (get_type (res) == global_binding_type)
    {
      objhdr *p = get_header (res);
      if (p -> u.cons_val.car_cell == sym)
        return (p);
    }
  return (NULL);
}

obj symbol_value (obj sym)
{
  if (sym <= obj_T)
    return (sym);
  {
    obj *p = find_lexical_binding (sym);
    if (p)
      return (*p);
  }
  {
    objhdr *p = find_global_binding (sym);
    if (p)
      return (p -> u.cons_val.cdr_cell);
  }

  return (sym);
}


obj set_symbol_value (obj sym, obj val)
{
  if (sym <= obj_T)
    throw_error (bad_obj);
  {
    obj *p = find_lexical_binding (sym);
    if (p)
      return (*p = val);
  }
  {
    objhdr *p = find_global_binding (sym);
    if (p)
    {
      if (sym == val)
        p -> xtype = unallocated_type;
      else
        p -> u.cons_val.cdr_cell = val;
      return (val);
    }
  }
  {
    objhdr *p;
    new_object (global_binding_type, &p);
    p -> u.cons_val.car_cell = sym;
    return (p -> u.cons_val.cdr_cell = val);
  }
}
