#include <stdio.h>

#include "io.h"
#include "obj.h"

uint8_t readc (void)
{
  return (getchar ());
}

void printc (uint8_t ch)
{
  putchar (ch);
}

obj fn_read (obj *argv)
{
  uint8_t ch1;
  while ((ch1 = readc ()) <= ' ')
    ;
  return (ch1);
}


static void print1 (obj o)
{
  printf ("%d\n", o);
}

obj fn_print (obj *argv)
{
  uint16_t argc = *argv++;
  while (argc--)
    print1 (*argv++);
  return (obj_NIL);
}
