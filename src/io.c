#include <stdio.h>
#if USE_LINUX
#include <stdlib.h>
#include "not-arduino.h"
#else
#include <Arduino.h>
#endif

#include "cons.h"
#include "integer.h"
#include "io.h"
#include "obj.h"
#include "symbols.h"


bool slow_output;

#if USE_LINUX
uint8_t readc (void)
{
  return (getchar ());
}

int16_t peekc (void)
{
  int16_t ch = getchar ();
  ungetc (ch, stdin);
  return (ch);
}

void pushbackc (uint8_t ch)
{
  ungetc (ch, stdin);
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
    MSG (no_fdefn);
    MSG (no_mem);
#undef MSG
  }
  slow_output = true;
  fprintf (stderr, "%s(%d): %s\n", file, line, msg);
  exit (1);
}

static obj read_token (uint8_t ch1)
{
  uint8_t spelling [MAX_TOKEN];
  uint8_t len = 0;
  for (;;)
  {
    spelling [len] = ch1;
    if ((len += 1) == MAX_TOKEN)
      break;
    if ((ch1 = readc ()) <= ' ' ||
	ch1 == ';' ||
	ch1 == '(' ||
	ch1 == ')')
    {
      pushbackc (ch1);
      break;
    }
  }
  bool neg = (len > 1) && (spelling [0] == '-');
  uint8_t i = neg;
  int32_t tot = 0;
 
  for (; i < len; i += 1)
  {
    if ((ch1 = spelling [i] - '0') > 9)
      return (find_symbol (spelling, len));
    tot = tot * 10 + ch1;
  }
  if (neg)
    tot = - tot;
  return (create_int (tot));
}

static void skip_blanks (void)
{
  for (;;)
  {
    int16_t ch1 = readc ();
    if (ch1 == ';')
    {
      while ((ch1 = readc ()) != '\r' && ch1 != '\n')
	;
    }
    else if (ch1 > ' ')
    {
      pushbackc (ch1);
      return;
    }
  }
}

static obj nreverse (obj x)
{
  obj prev = obj_NIL;
  while (x != obj_NIL)
  {
    objhdr *p = get_header (x);
    obj tmp = p -> u.cons_val.cdr_cell;
    p -> u.cons_val.cdr_cell = prev;
    prev = x;
    x = tmp;
  }
  return (prev);
}

static obj read_list (void)
{
  obj res = obj_NIL;
  for (;;)
  {
    skip_blanks ();
    uint8_t ch;
    if ((ch = readc ()) == ')')
      return (nreverse (res));

    pushbackc (ch);
    objhdr *p = (res != obj_NIL) ? get_header (res) : NULL;

    // protect res across the cons() call
    if (p)
      p -> flags |= gc_fixed;
    {
      res = cons (internal_read (), res);
    }
    if (p)
      p -> flags &= ~gc_fixed;

  }
}

obj internal_read (void)
{
  skip_blanks ();
  uint8_t ch1 = readc ();
  if (ch1 == '\'')
    return (cons (obj_QUOTE, cons (internal_read (), obj_NIL)));

  if (ch1 == '(')
    return (read_list ());

  return (read_token (ch1));
}

obj fn_read (obj args)
{
  (void) args;
  return (internal_read ());
}


void print1 (obj o)
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
    printc ('|');
    while (len--)
      printc (*p++);
    printc ('|');
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

  case cons_type:
  {
    printc ('(');
    while (get_type (o) == cons_type)
    {
      obj car;
      decons (o, &car, &o);
      print1 (car);
      if (o != obj_NIL)
	printc (' ');
    }
    if (o != obj_NIL)
    {
      printc ('.');
      printc (' ');
      print1 (o);
    }
    printc (')');
    break;
  }

  case int_type:
    printf ("%ld", (long)get_int_val (o));
    break;

  default:
    printf ("#<obj 0x%04x, type %d>", o, get_type (o));
    break;
  }
}

obj fn_print (obj args)
{
  obj *p = get_header (args) -> u.array_val;
  uint16_t argc = *p++;
  while (argc--)
    print1 (*p++);
  printc ('\n');
  return (obj_NIL);
}

obj fn_readchar (obj args)
{
  (void) args;
  return (FIRST_CHAR + readc ());
}

obj fn_peekchar (obj args)
{
  (void) args;
  int ch = peekc ();
  if (ch >= 0)
    return (FIRST_CHAR + (ch & 0xFF));
  else
    return (obj_NIL);
}
