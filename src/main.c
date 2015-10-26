#if USE_STDIO
#include <stdio.h>
#include <stdlib.h>
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
  init_memory ();

  for (;;)
  {
    fprintf (stderr, "t is 0x%04x\n", find_symbol ((uint8_t *) "t", 1));
    fprintf (stderr, "xyzzy is 0x%04x\n", find_symbol ((uint8_t *) "xyzzy", 5));
    fprintf (stderr, "plugh is 0x%04x\n", find_symbol ((uint8_t *) "plugh", 5));
    obj x [2];
    x [0] = 1;
    x [1] = fn_read (NULL);
    obj printer = find_symbol ((uint8_t *) "print", 5);
    built_in_fn f = get_rom_header (printer) -> global_fn;
    f (x);
    x [1] = find_symbol ((uint8_t *) "cons", 4);
    f (x);
    x [0] = 1;
    x [1] = find_symbol ((uint8_t *) "xyzzy", 5);
    f (x);
  }
  return (0);
}
