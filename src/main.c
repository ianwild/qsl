#include <setjmp.h>

#include "announce.h"
#include "compiler.h"
#include "dbg.h"
#include "eval.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "stack.h"

START_IMPLEMENTATION

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
END_IMPLEMENTATION

#if ! REPL_IS_MAIN
#if __cplusplus && WITH_NAMESPACE
  #define repl  QSL::repl
#endif

#if FROZEN_BOOTSTRAP
#include <stdlib.h>
#include <stdio.h>

static const enum typecode reallocated = last_type_code;

static void pack_ram_headers (void)
{
  do_gc ();
  announce (ann_shutting_down);
  do_gc ();
  obj dest = FIRST_RAM_OBJECT;
  obj src = last_allocated_object;
  while (dest < src)
  {
    objhdr *dptr = get_header (dest);
    if (GET_TYPE (dptr) != unallocated_type)
    {
      dest += 1;
      continue;
    }
    objhdr *sptr = get_header (src);
    if (GET_TYPE (sptr) == unallocated_type)
    {
      src -= 1;
      continue;
    }
    *dptr = *sptr;
    sptr -> control = reallocated;
    sptr -> u.cons_val.car_cell = dest;
    dest += 1;
    src -= 1;
  }
  while (get_type (src) == unallocated_type)
    src -= 1;
  last_allocated_object = src;
}

static obj follow (obj o)
{
  if (o >= FIRST_RAM_OBJECT && o <= last_allocated_object)
  {
    objhdr *p = get_header (o);
    if (GET_TYPE (p) == reallocated)
      return (p -> u.cons_val.car_cell);
  }
  return (o);
}

static void write_frozen_state (void)
{
  pack_ram_headers ();
  FILE *bytes = fopen ("bytes", "w");
  int byte_count = 0;
  FILE *words = fopen ("words", "w");
  int word_count = 0;
  FILE *objects = fopen ("objects", "w");
  //int object_count = 0;

  for (obj o = FIRST_RAM_OBJECT; o <= last_allocated_object; o += 1)
  {
    objhdr *p = get_header (o);
    fprintf (objects, "/* %d */ ", o);
    uint8_t *spelling = NULL;
    obj *body = NULL;
    switch (GET_TYPE (p))
    {
    case unallocated_type:
      fprintf (objects, "{.control = unallocated_type},\n");
      break;

    case closure_type:
      fprintf (objects,
               "{.control = closure_type, "
               ".u.closure_val = {.environment = %d, .lambda_obj = %d}},\n",
               follow (p -> u.closure_val.environment),
               follow (p -> u.closure_val.lambda_obj));
      break;

    case lambda_type:
      fprintf (objects,
               "{.control = lambda_type, "
               ".u.lambda_body = {.opcodes = %d, .constants = %d}},\n",
               follow (p -> u.lambda_body.opcodes),
               follow (p -> u.lambda_body.constants));
      break;

    case symbol_type:
      spelling = p -> u.symbol_val.spelling;
      fprintf (objects,
               "{.control = symbol_type, "
               ".u.symbol_val = {.spelling = %d, .global_fn = %d}},\n",
               byte_count, follow (p -> u.symbol_val.global_fn));
      break;

    case cons_type:
      fprintf (objects,
               "{.control = cons_type, "
               ".u.cons_val = {.car_cell = %d, .cdr_cell = %d}},\n",
               follow (p -> u.cons_val.car_cell),
               follow (p -> u.cons_val.cdr_cell));
      break;

    case global_binding_type:
      fprintf (objects,
               "{.control = global_binding_type, "
               ".u.cons_val = {.car_cell = %d, .cdr_cell = %d}},\n",
               follow (p -> u.cons_val.car_cell),
               follow (p -> u.cons_val.cdr_cell));
      break;

    case int_type:
      fprintf (objects,
               "{.control = int_type, .u.int_val = %d},\n",
               p -> u.int_val);
      break;

    case string_type:
      spelling = p -> u.string_val;
      fprintf (objects,
               "{.control = string_type, .u.string_val = %d},\n",
               byte_count);
      break;

    case array_type:
      body = p -> u.array_val;
      fprintf (objects,
               "{.control = array_type, .u.array_val = %d},\n",
               word_count);
      break;

    case environment_type:
      body = p -> u.array_val;
      fprintf (objects,
               "{.control = environment_type, .u.array_val = %d},\n",
               word_count);
      break;

    default:
      fprintf (objects, "{.control = weird %d},\n", GET_TYPE (p));
      break;
    }
    if (spelling)
    {
      int len = *spelling++;
      fprintf (bytes, "/* %d->%d */ %d, ", o, byte_count, len);
      for (int i = 0; i < len; i += 1)
        fprintf (bytes, "%d, ", *spelling++);
      fprintf (bytes, "\n");
      byte_count += len + 1;
    }
    if (body)
    {
      int len = *body++;
      fprintf (words, "/* %d->%d */ %d, ", o, word_count, len);
      for (int i = 0; i < len; i += 1)
        fprintf (words, "%d, ", follow (*body++));
      fprintf (words, "\n");
      word_count += len + 1;
    }
  }
  fclose (objects);
  fclose (words);
  fclose (bytes);
}
#endif

int main (void)
{
  #if FROZEN_BOOTSTRAP
  atexit (write_frozen_state);
  #endif
  repl ();
  return (0);
}
#endif
