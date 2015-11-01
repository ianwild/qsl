#include <stdio.h>

#if __cplusplus
extern "C" {
#endif

#include "eval.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "symbols.h"
#include "target.h"

#ifdef __cplusplus
}
#endif

#if TARGET_ARDUINO
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
  stderr = stdout = fdevopen (stdout_write, 0);
#endif
  init_memory ();

  for (;;)
  {
    printf ("\nqsl> ");
    do_gc ();
    obj x = internal_read ();
    objhdr *p = (get_type (x) == cons_type) ? get_header (x) : NULL;

    // protect x across the eval_internal() call
    if (p)
      p -> flags |= gc_fixed;
    {
      x = eval_internal (x);
    }
    if (p)
      p -> flags &= ~gc_fixed;

    printf ("\n= ");
    print1 (x);
  }
  return (0);
}

