#include "compiler.h"
#include "embedded.h"
#include "eval.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "stack.h"
#include "symbols.h"
#include "target.h"


int main (void)
{
#if TARGET_ARDUINO
  embed_init ();
#endif

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
      compile_opcode (opRETURN);
      compiler_report ();
      printf ("stack depth: %u\n", get_stack_depth ());
      interpret_bytecodes ();
      x = pop_arg ();
#endif
    }
    if (p)
      p -> flags &= ~gc_fixed;

    print_rom_string (PSTR ("\n= "));
    print1 (x);
    printf ("stack depth: %u\n", get_stack_depth ());
  }
  return (0);
}

