#include <stdlib.h>

#include "io.h"

int main (void)
{
  for (;;)
  {
    obj x [2];
    x [0] = 1;
    x [1] = read (NULL);
    print (x);
  }
  return (0);
}
