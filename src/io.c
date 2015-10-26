#include <stdio.h>
#if USE_LINUX
#include <stdlib.h>
#include "not-arduino.h"
#else
#include <Arduino.h>
#endif

#include "io.h"
#include "obj.h"

#if USE_LINUX
uint8_t readc (void)
{
  return (getchar ());
}

void printc (uint8_t ch)
{
  putchar (ch);
}
#endif

void (throw_error) (enum errcode e, char *file, int line)
{
  char *msg = "weird";
  switch (e)
  {
#define MSG(x) case x: msg = #x; break
    MSG (no_error);
    MSG (bad_type);
    MSG (bad_obj);
    MSG (bad_argc);
    MSG (div_by_zero);
    MSG (no_mem);
#undef MSG
  }
  fprintf (stderr, "%s(%d): %s\n", file, line, msg);
  exit (1);
}

obj fn_read (obj args)
{
  (void) args;
  uint8_t ch1;
  while ((ch1 = readc ()) <= ' ')
    ;
  if (ch1 == 0xFF)
    return (obj_NIL);
  return (ch1 + FIRST_CHAR);
}


static void print1 (obj o)
{
  switch (get_type (o))
  {
  case char_type:
    printc ('#');
    printc ('\\');
    printc (o - FIRST_CHAR);
    break;

  case string_type:
  case symbol_type:
  {
    uint16_t len;
    uint8_t *p = get_spelling (o, &len);
    while (len--)
      printc (*p++);
    break;
  }

  case rom_symbol_type:
  {
    uint16_t len;
    const uint8_t *p = get_rom_spelling (o, &len);
    while (len--)
      printc (pgm_read_byte_near (p++));
    break;
  }

  default:
    break;
  }
  printc ('\n');
}

obj fn_print (obj args)
{
  obj *p = get_header (args) -> u.array_val;
  uint16_t argc = *p++;
  while (argc--)
    print1 (*p++);
  return (obj_NIL);
}
