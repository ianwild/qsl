#include <string.h>

#include "dbg.h"
#include "eval.h"
#include "misc.h"
#include "obj.h"
#include "symbols.h"

START_IMPLEMENTATION


obj find_symbol (uint8_t *spelling, uint16_t len)
{
  obj sym = 0;

  for (; sym <= LAST_ROM_OBJECT; sym += 1)
  {
    uint16_t rom_len;
    const uint8_t *rom_spelling = get_rom_spelling (sym, &rom_len);
    if (compare_strings (spelling, len, false,
                         rom_spelling, rom_len, true) == 0)
      return (sym);
  }

  #if FROZEN_OBJECT_COUNT > 0
  for (; sym <= LAST_FROZEN_OBJECT; sym += 1)
  {
    if (get_type (sym) == symbol_type)
    {
      const uint8_t *frozen_spelling = get_frozen_spelling (sym);
      uint16_t frozen_len = pgm_read_byte_near (frozen_spelling++);
      if (compare_strings (spelling, len, false,
                           frozen_spelling, frozen_len, true) == 0)
        return (sym);
    }
  }
  #endif

  for (; sym <= last_allocated_object; sym += 1)
  {
    if (get_type (sym) == symbol_type)
    {
      uint16_t ram_len;
      const uint8_t *ram_spelling = get_spelling (sym, &ram_len);
      if (compare_strings (spelling, len, false,
                           ram_spelling, ram_len, false) == 0)
        return (sym);
    }
  }

  sym = new_extended_object (symbol_type, len);
  uint16_t dummy;
  uint8_t *new_spelling = get_spelling (sym, &dummy);
  memcpy (new_spelling, spelling, len);
  return (sym);
}

static obj *find_lexical_binding (obj sym, bool *is_frozen)
{
  obj env = current_environment;
  while (env != obj_NIL)
  {
    #if FROZEN_OBJECT_COUNT
    if (env >= FIRST_FROZEN_OBJECT && env <= LAST_FROZEN_OBJECT)
    {
      const obj *p = get_frozen_body (env);
      uint16_t len = (uint16_t) pgm_read_word_near (p++) - 1;
      env = (obj) pgm_read_word_near (p++);            // parent environment
      while (len)
      {
        if ((obj) pgm_read_word_near (p) == sym)
        {
          *is_frozen = true;
          return ((obj *) p + 1);
        }
        p += 2;
        len -= 2;
      }
      continue;
    }
    #endif
    obj *p = get_header (env) -> u.array_val;
    uint16_t len = (uint16_t) *p++ - 1;
    env = *p++;                 // parent environment
    while (len)
    {
      if (*p == sym)
      {
        *is_frozen = false;
        return (p + 1);
      }
      p += 2;
      len -= 2;
    }
  }
  return (NULL);
}

static objhdr *find_global_binding (obj sym, bool *is_frozen)
{
  #if FROZEN_OBJECT_COUNT
  for (obj res = FIRST_FROZEN_OBJECT; res <= LAST_FROZEN_OBJECT; res += 1)
    if (get_type (res) == global_binding_type)
    {
      const frozen_hdr *p = get_frozen_header (res);
      if (pgm_read_word_near (&p -> u.cons_val.car_cell) == sym)
      {
        *is_frozen = true;
        return ((objhdr *) p);
      }
    }
  #endif
  for (obj res = FIRST_RAM_OBJECT; res <= last_allocated_object; res += 1)
    if (get_type (res) == global_binding_type)
    {
      objhdr *p = get_header (res);
      if (p -> u.cons_val.car_cell == sym)
      {
        *is_frozen = false;
        return (p);
      }
    }
  return (NULL);
}

obj symbol_value (obj sym, bool *is_frozen)
{
  if (sym <= obj_T)
    return (sym);
  {
    obj *p = find_lexical_binding (sym, is_frozen);
    if (p)
    {
      #if FROZEN_OBJECT_COUNT
      if (*is_frozen)
        return ((obj) pgm_read_word_near (p));
      #endif
      return (*p);
    }
  }
  {
    objhdr *p = find_global_binding (sym, is_frozen);
    if (p)
    {
      #if FROZEN_OBJECT_COUNT
      if (*is_frozen)
      {
        const frozen_hdr *p0 = (frozen_hdr *)p;
        return ((obj) pgm_read_word_near (&p0 -> u.cons_val.cdr_cell));
      }
      #endif
      return (p -> u.cons_val.cdr_cell);
    }
  }

  return (obj_NIL);
}

obj set_symbol_value (obj sym, obj val)
{
  if (sym <= obj_T)
    throw_error (bad_obj);
  bool is_frozen;
  {
    obj *p = find_lexical_binding (sym, &is_frozen);
    if (p)
    {
      if (is_frozen)
        throw_error (read_only);
      return (*p = val);
    }
  }
  {
    objhdr *p = find_global_binding (sym, &is_frozen);
    if (p)
    {
      if (is_frozen)
        throw_error (read_only);
      if (!val)
        p -> control = unallocated_type;
      else
        p -> u.cons_val.cdr_cell = val;
      return (val);
    }
  }
  if (val)
  {
    objhdr *p;
    new_object (global_binding_type, &p);
    p -> u.cons_val.car_cell = sym;
    p -> u.cons_val.cdr_cell = val;
  }
  return (val);
}

END_IMPLEMENTATION
