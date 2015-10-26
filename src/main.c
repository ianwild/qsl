#include <stdlib.h>

#include "io.h"
#include "obj.h"

int main (void)
{
  for (;;)
  {
    obj x [2];
    x [0] = 1;
    x [1] = fn_read (NULL);
    built_in_fn f = get_rom_header ((x [1] > 128) ? x [1] : 18) -> global_fn;
    f (x);
  }
  return (0);
}
