#include <setjmp.h>
#include <string.h>

#include "announce.h"
#include "buffer-limits.h"
#include "cons.h"
#include "dbg.h"
#include "gc.h"
#include "integer.h"
#include "io.h"
#include "obj.h"
#include "serial_io.h"
#include "stack.h"
#include "symbols.h"


#if WITH_RC_SCRIPT

static const PROGMEM uint8_t rc_text [] =
  // define a readable newline
  "(setq nl (code-char 10))"

  #if ! TARGET_ARDUINO
  // stub out the functions defined in hardware.cpp
  "(defun pin () ())"
  "(defun on-tick () ())"
  "(defun on-serial () ())"
  "(defun wait-for-event ())"
  "(defun next-event ())"
  #endif
  "";

static const uint8_t *rc_script = rc_text;

#endif


static_assert (MAX_TOKEN_LENGTH > 8, "token buffer too small");

bool slow_output;

jmp_buf reset;

static obj io_buffer;
static uint8_t *spelling;
static int16_t latch = -1;

void io_announce (enum announcement ann)
{
  switch (ann)
  {
  case ann_gc_starting:
    io_buffer = obj_NIL;
    spelling = NULL;
    break;

  default:
    break;
  }
}


static void allocate_io_buffers (void)
{
  if (! spelling)
  {
    io_buffer = new_extended_object (string_type, MAX_TOKEN_LENGTH);
    spelling = get_spelling (io_buffer, NULL);
  }
}

uint8_t readc (void)
{
  if (latch >= 0)
  {
    uint8_t ch = latch;
    latch = -1;
    return (ch);
  }

#if WITH_RC_SCRIPT
  if (rc_script)
  {
    uint8_t ch = pgm_read_byte_near (rc_script++);
    if (ch)
      return (ch);
    rc_script = NULL;
  }
#endif

#if TARGET_ARDUINO
  return (serial_readc ());
#else
  int ch = getchar ();
  if (ch == EOF)
    exit (0);
  return (ch);
#endif
}

int peekc (void)
{
  if (latch >= 0)
    return (latch);

#if WITH_RC_SCRIPT
  if (rc_script)
  {
    uint8_t ch = pgm_read_byte_near (rc_script);
    if (ch)
      return (latch = ch);
    rc_script = NULL;
  }
#endif

#if TARGET_ARDUINO
  return (latch = serial_peekc ());
#else
  return (latch = getchar ());
#endif
}

void pushbackc (uint8_t ch)
{
  latch = ch;
}

void printc (uint8_t ch)
{
#if TARGET_ARDUINO
  serial_printc (ch);
#else
  putchar (ch);
#endif
}


static void print_rom_string (const char *p)
{
  uint8_t ch;

  while ((ch = (uint8_t) pgm_read_byte_near (p++)) != 0)
    printc (ch);
}

void print_prompt (void)
{
#if WITH_RC_SCRIPT
  if (rc_script && pgm_read_byte_near (rc_script))
    return;
#endif
  print_rom_string (PSTR ("\nqsl> "));
}

void print_result (obj o)
{
#if WITH_RC_SCRIPT
  if (rc_script)
    return;
#endif
  print_rom_string (PSTR ("\n= "));
  print1 (o);
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
  slow_output = false;
  longjmp (reset, 1);
}

static obj read_token (uint8_t ch1)
{
  allocate_io_buffers ();
  uint8_t len = 0;
  for (;;)
  {
    spelling [len] = ch1;
    if ((len += 1) == MAX_TOKEN_LENGTH)
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
    if ((len += 1) == MAX_TOKEN_LENGTH)
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
      FIX_OBJ (p);
    {
      res = cons (internal_read (), res);
    }
    if (p)
      RELEASE_OBJ (p);
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

static void print_quoted (uint8_t q, obj o)
{
  uint16_t len;
  uint8_t *p = get_spelling (o, &len);
#if PRINT_WITH_QUOTES
  printc (q);
  while (len--)
  {
    if (*p == q || *p == '\\')
      printc ('\\');
    printc (*p++);
  }
  printc (q);
#else
  (void) q;
  while (len--)
    printc (*p++);
#endif
}

void print1 (obj o)
{
  switch (get_type (o))
  {
  case char_type:
#if PRINT_WITH_QUOTES
    printc ('?');
#endif
    printc (o - FIRST_CHAR);
    break;

  case string_type:
    print_quoted ('"', o);
    break;

  case symbol_type:
    print_quoted ('|', o);
    break;

  case rom_symbol_type:
  {
    uint16_t len;
#if PRINT_WITH_QUOTES
    printc ('|');
#endif
    const uint8_t *p = get_rom_spelling (o, &len);
    while (len--)
      printc (pgm_read_byte_near (p++));
#if PRINT_WITH_QUOTES
    printc ('|');
#endif
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
