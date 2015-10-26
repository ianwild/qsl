#include <stdio.h>
#if USE_LINUX
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

#if ! USE_LINUX
static int stdout_write (char ch, FILE *dummy)
{
  (void) dummy;
  printc (ch);
}
#endif

int main (void)
{
#if ! USE_LINUX
  init ();
  Serial.begin (9600);
  stderr = stdout = fdevopen (stdout_write, 0);
#endif
  init_memory ();

  for (;;)
  {
    printf ("t is 0x%04x\n", find_symbol ((uint8_t *) "t", 1));
    printf ("xyzzy is 0x%04x\n", find_symbol ((uint8_t *) "xyzzy", 5));
    printf ("plugh is 0x%04x\n", find_symbol ((uint8_t *) "plugh", 5));

    obj x = new_extended_object (array_type, 1);
    get_header (x) -> u.array_val [1] = fn_read (x);
    obj printer = find_symbol ((uint8_t *) "print", 5);
    built_in_fn f = (built_in_fn) pgm_read_word_near (&get_rom_header (printer) -> global_fn);
    f (x);
  }
  return (0);
}

