#if USE_STDIO
#include <stdio.h>
#include <stdlib.h>
#include "not-arduino.h"
#else
#include <Arduino.h>
#endif

#if __cplusplus
extern "C" {
#endif

#include "io.h"
#include "obj.h"
#include "symbols.h"

#ifdef __cplusplus
}
#endif

int main (void)
{
#if ! USE_STDIO
  init ();
  Serial.begin (9600);
#endif
  init_memory ();

  for (;;)
  {
    char buf [32];
    sprintf (buf, "t is 0x%04x", find_symbol ((uint8_t *) "t", 1));
    msg (buf);
    sprintf (buf, "xyzzy is 0x%04x", find_symbol ((uint8_t *) "xyzzy", 5));
    msg (buf);
    sprintf (buf, "plugh is 0x%04x", find_symbol ((uint8_t *) "plugh", 5));
    msg (buf);
    obj x = new_extended_object (array_type, 1);
    get_header (x) -> u.array_val [1] = fn_read (x);
    obj printer = find_symbol ((uint8_t *) "print", 5);
    built_in_fn f = (built_in_fn) pgm_read_word_near (&get_rom_header (printer) -> global_fn);
    f (x);
  }
  return (0);
}

