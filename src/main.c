#include "compiler.h"
#include "eval.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "symbols.h"
#include "target.h"


#if 0 && TARGET_ARDUINO
static int stdout_write (char ch, FILE *dummy)
{
  (void) dummy;
  printc (ch);
  return (1);
}
#endif

int main (void)
{
#if TARGET_ARDUINO
  init ();
  Serial.begin (9600);
  // stderr = stdout = fdevopen (stdout_write, 0);
#endif
  init_memory ();

  for (;;)
  {
    print_rom_string (PSTR ("\nqsl> "));
    do_gc ();
    obj x = internal_read ();
    objhdr *p = (get_type (x) == cons_type) ? get_header (x) : NULL;

    // protect x across the eval_internal() call
    if (p)
      p -> flags |= gc_fixed;
    {
#if 0
      x = eval_internal (x);
#else
      compiler_init ();
      compile_expression (x, true);
      compiler_report ();
      compiler_init ();
      compile_expression (x, false);
      compiler_report ();
#endif
    }
    if (p)
      p -> flags &= ~gc_fixed;

    print_rom_string (PSTR ("\n= "));
    print1 (x);
  }
  return (0);
}

