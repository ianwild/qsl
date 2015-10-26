#if USE_STDIO
#include <stdio.h>
#include <string.h>
#define pgm_read_byte_near(x) (*(x))
#else
#include <Arduino.h>
#endif

#include "obj.h"
#include "rom-symbols.h"
#include "symbols.h"

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
