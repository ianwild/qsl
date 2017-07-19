#include <string.h>

#include "cons.h"
#include "dbg.h"
#include "gc-hooks.h"
#include "integer.h"
#include "io.h"
#include "obj.h"
#include "rom-symbols.h"
#include "stack.h"
#include "symbols.h"
#include "target.h"

bool slow_output;

static obj io_buffer;
static uint8_t *spelling;

void free_io_buffers (void)
{
  io_buffer = obj_NIL;
  spelling = NULL;
}

static void allocate_io_buffers (void)
{
  if (! spelling)
  {
    io_buffer = new_extended_object (string_type, MAX_TOKEN);
    spelling = get_spelling (io_buffer, NULL);
  }
}

#if ! TARGET_ARDUINO
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

void print_rom_string (const char *p)
{
  uint8_t ch;

  while ((ch = (uint8_t) pgm_read_byte_near (p++)) != 0)
    printc (ch);
}

void print_int (int32_t n0)
{
  bool neg = (n0 < 0);
  uint32_t n = neg ? -n0 : n0;
  uint8_t buf [10];
  uint8_t *p = buf;
  while (n >= 10)
  {
    *p++ = n % 10;
    n /= 10;
  }
  *p++ = n;
  if (neg)
    printc ('-');
  while (p > buf)
    printc ('0' + *--p);
}

void (throw_error) (enum errcode e, const char *file, int line)
{
  const char *msg = PSTR ("weird");
  switch (e)
  {
#define MSG(x) case x: msg = PSTR (#x); break
    MSG (no_error);
    MSG (bad_type);
    MSG (bad_obj);
    MSG (bad_argc);
    MSG (bad_idx);
    MSG (div_by_zero);
    MSG (no_fdefn);
    MSG (no_mem);
    MSG (compiler_error);
#undef MSG
  }
  slow_output = true;
  print_rom_string (file);
  printc ('(');
  print_int (line);
  print_rom_string (PSTR ("): "));
  print_rom_string (msg);
  printc ('\n');
  exit (1);
}

static obj read_token (uint8_t ch1)
{
  allocate_io_buffers ();
  uint8_t len = 0;
  for (;;)
  {
    spelling [len] = ch1;
    if ((len += 1) == MAX_TOKEN)
      break;
    if ((ch1 = readc ()) <= ' ' ||
        ch1 == ';' ||
        ch1 == '\'' ||
        ch1 == '"' ||
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

static obj read_string (uint8_t quote)
{
  allocate_io_buffers ();
  uint16_t len = 0;
  for (;;)
  {
    uint8_t ch = readc ();
    if (ch == quote)
      break;
    else if (ch == '\\')
      ch = readc ();
    spelling [len] = ch;
    if ((len += 1) == MAX_TOKEN)
      break;
  }
  obj res = new_extended_object ((quote == '"' ? string_type : symbol_type),
                                 len);
  uint8_t *p = get_spelling (res, NULL);
  memcpy (p, spelling, len);
  return (res);
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

static obj nreverse (obj x, obj prev)
{
  while (x != obj_NIL)
  {
    objhdr *p = get_header (x);
    obj tmp = p -> u.cons_val.cdr_cell;
    p -> u.cons_val.cdr_cell = prev;
    prev = x;
    x = tmp;
  }
  return (working_root = prev);
}

static obj read_list (void)
{
  obj res = obj_NIL;

  for (;;)
  {
    skip_blanks ();
    uint8_t ch;
    switch (ch = readc ())
    {
    case ')':
      return (nreverse (res, obj_NIL));

    case '.':
    {
      obj last = internal_read ();
      skip_blanks ();
      if ((ch = readc ()) != ')')
        pushbackc (ch);
      return (nreverse (res, last));
    }
    }

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

  switch (ch1)
  {
  case '\'':
    return (cons (obj_QUOTE, cons (internal_read (), obj_NIL)));

  case '?':
    return (FIRST_CHAR + readc ());

  case '"':
  case '|':
    return (read_string (ch1));

  case '(':
    return (read_list ());

  default:
    return (read_token (ch1));
  }
}

obj fn_read (uint8_t *argc)
{
  (void) argc;
  return (internal_read ());
}


void print1 (obj o)
{
  switch (get_type (o))
  {
  case char_type:
    printc ('?');
    printc (o - FIRST_CHAR);
    break;

  case string_type:
  {
    uint16_t len;
    uint8_t *p = get_spelling (o, &len);
    printc ('"');
    while (len--)
    {
      if (*p == '"' || *p == '\\')
        printc ('\\');
      printc (*p++);
    }
    printc ('"');
    break;
  }

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

  case array_type:
  case environment_type:
  {
    printc ('#');
    printc ('(');
    obj *p = get_header (o) -> u.array_val;
    uint16_t n = *p++;
    while (n)
    {
      print1 (*p++);
      n -= 1;
      if (n)
        printc (' ');
    }
    printc (')');
    break;
  }

  case int_type:
    print_int (get_int_val (o));
    break;

  default:
    print_rom_string (PSTR ("#<obj "));
    print_int (o);
    print_rom_string (PSTR (", type "));
    print_int (get_type (o));
    printc ('>');
    break;
  }
}

obj fn_print (uint8_t *argc)
{
  uint8_t n = *argc;
  while (n)
    print1 (get_arg (n -= 1));
  printc ('\n');
  return (obj_NIL);
}

obj fn_readchar (uint8_t *argc)
{
  (void) argc;
  return (FIRST_CHAR + readc ());
}

obj fn_peekchar (uint8_t *argc)
{
  (void) argc;
  int ch = peekc ();
  if (ch >= 0)
    return (FIRST_CHAR + (ch & 0xFF));
  else
    return (obj_NIL);
}

obj fn_at (uint8_t *argc)
{
  adjust_argc (argc, 1);
  obj n = get_arg (0);
  if (n >= obj_ZERO)
  {
    obj o = (obj) (n - obj_ZERO);
    if (o <= last_allocated_object)
      return (o);
  }
  return (n);
}
