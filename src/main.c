#include "compiler.h"
#include "dbg.h"
#include "embedded.h"
#include "eval.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "stack.h"


int main (void)
{
  stack_reinit ();

#if TARGET_ARDUINO
  embed_init ();
#endif

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
