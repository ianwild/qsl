#include <setjmp.h>

#include "announce.h"
#include "compiler.h"
#include "dbg.h"
#include "eval.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "stack.h"


int main (void)
{
  extern jmp_buf reset;
  announce (ann_startup);

  switch (setjmp (reset))
  {
  case 1:
    announce (ann_computation_aborted);
    break;
  }

  for (;;)
  {
    print_rom_string (PSTR ("\nqsl> "));
    do_gc ();
    obj x = internal_read ();

    x = compile_top_level (x);
    print_stack_depth ();
    x = interpret_top_level (x);
    dump_stack ();

    print_rom_string (PSTR ("\n= "));
    print1 (x);
  }
  return (0);
}
