#include <setjmp.h>

#include "announce.h"
#include "compiler.h"
#include "dbg.h"
#include "eval.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "stack.h"

START_EXTERN_C

#if REPL_IS_MAIN
void repl (void)
#else
static void repl (void)
#endif
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
    print_prompt ();
    do_gc ();
    obj x = internal_read ();

    x = compile_top_level (x);
    print_stack_depth ();
    x = interpret_top_level (x);
    dump_stack ();

    print_result (x);
  }
}
END_EXTERN_C

#if ! REPL_IS_MAIN
#if __cplusplus && WITH_NAMESPACE
  #define repl  QSL::repl
#endif

int main (void)
{
  repl ();
  return (0);
}
#endif
