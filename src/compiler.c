#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "announce.h"
#include "buffer-limits.h"
#include "compiler.h"
#include "cons.h"
#include "dbg.h"
#include "fexprs.h"
#include "gc.h"
#include "io.h"
#include "obj.h"
#include "stack.h"

START_IMPLEMENTATION

static obj prog_obj;
static uint8_t *prog;
static uint8_t prog_length;
static uint8_t longest_prog;

static obj constants_obj;
static obj *constants;
static uint8_t const_length;
static uint8_t longest_const;

/*
  The `compile_top_level()` function takes an expression and generates
  two vectors, one a `string_type` of byte-codes, the other an
  `array_type` of objects referenced by the byte-codes.  This split
  allows the garbage collector to mark the dependent objects
  efficiently without needing to parse the opcodes.  Usually, this can
  be done with only a small amount of recursion, but...

  Problem: what happens to any nested (lambda ...) or (defun ...)
  expressions during compilation?  These will need their own vectors
  of opcodes and constants, and, in effect, the whole compiler state.
  I _really_ don't want to recurse to that degree.

  A "compiled lambda" is only a skeleton: it needs to be combined at
  run time with an environment before it becomes usable, which is what
  `opCREATE_CLOSURE` does.  That means a closure's `environment`
  pointer is up for grabs until the _outer_ lambda gets executed...

  _Warning: Trickery Alert_

  We create a sort of "bookmark", a "compiled lambda" object whose
  "body" field is the (nested) _list to be compiled_ and whose
  "environment" field is a marker (`obj_T`, in fact) saying "not yet
  compiled".  Once the outer clause has been compiled,
  `compile_top_level()` loops through memory, looking for these
  bookmarks and feeding them to `compile_pending_expression()`.  This
  may, in turn, generate more bookmarks, which will themselves be
  caught at a later iteration.

  (In fact, the looping starts a little earlier than you might expect.
  The original expression given to `compile_top_level()` is wrapped in
  one of these bookmarks, and immediately becomes eligible for
  processing by `compile_pending_expression()`.)

  Of course the garbage collector needs a special case to understand
  that an environment of `obj_T` means "will _soon_ be in use", which
  means throwing an exception in the compiler needs to "un-special
  case" things (in `obj.c`'s announcement handler) or you'll get an
  infinite loop.
*/


static_assert (sizeof (enum opcodes) == 1,
               "opcodes too big for a byte");
static_assert (last_type_code <= typecode_mask,
               "too many types defined");
static_assert (MAX_OPCODES_PER_LAMBDA <= UINT8_MAX,
               "opcode limit too big to buffer");
static_assert ((MAX_OPCODES_PER_LAMBDA > 2 * MAX_LITERALS_PER_LAMBDA) &&
               (MAX_LITERALS_PER_LAMBDA >= 4),
               "literal limit not realistic");

#if TARGET_ARDUINO

static_assert (sizeof (objhdr) == 5, "objhdr should be five bytes");

#else

static_assert (sizeof (objhdr) & 1, "misaligned accesses might not work");
/*
  Okay - that one's a bit of a trick.

  An objhdr is a flag byte followed by one of:
     some uint16_t objects (so total size is odd); or
     a pointer (so total size is odd).

  If sizeof(objhdr) is EVEN, then, the compiler has added padding,
  which means it has decided to ignore the packed attribute, which
  probably means it doesn't believe the processor can handle
  misaligned uint16_t or pointer variables.

  Even if it gets far enough to need it, the compact_string_space()
  function is no respecter of alignment, so the first garbage
  collection will cause mysterious failures.  Better to head them off
  early.

*/
#endif

uint8_t get_longest_opcodes (void)
{
  return (longest_prog);
}

uint8_t get_longest_constants (void)
{
  return (longest_const);
}


void compiler_announce (enum announcement ann)
{
  switch (ann)
  {
  case ann_startup:
  case ann_clear_memory:
  case ann_computation_aborted:
  case ann_shutting_down:
    prog_obj = constants_obj = obj_NIL;
    // fall through

  case ann_gc_starting:
    prog = NULL;
    constants = NULL;
    want_obj (prog_obj);
    want_obj (constants_obj);
    break;

  default:
    break;
  }
}

static void compiler_init (void)
{
  if (! prog_obj)
    prog_obj = new_extended_object (string_type, MAX_OPCODES_PER_LAMBDA);
  if (! constants_obj)
    constants_obj = new_extended_object (array_type, MAX_LITERALS_PER_LAMBDA);
  prog = get_spelling (prog_obj, NULL);
  constants = wksp_obj_ptr (get_header (constants_obj) -> u.array_val) + 1;
  prog_length = 0;
  const_length = 0;
}

#if WITH_COMPILER_STATS
static void compiler_report (void)
{
  printf ("Constants:\n");
  for (unsigned i = 0; i < const_length; i += 1)
  {
    printf ("  %3u: %04x ", i, constants [i]);
    print1 (constants [i]);
    printf ("\n");
  }

  printf ("Opcodes:\n");
  for (unsigned i = 0; i < prog_length; i += 1)
    printf (" %02x", prog [i]);
  printf ("\n");
}
#else
#define compiler_report()
#endif

forward_jump declare_forward_jump (void)
{
#if TARGET_ARDUINO
  return (0);
#else
  forward_jump jmp = {0};
  return (jmp);
#endif
}


forward_jump insert_forward_jump (forward_jump jmp)
{
  uint8_t here = prog_length;
#if TARGET_ARDUINO
  compile_opcode (jmp);
  return (here);
#else
  compile_opcode (jmp.link);
  jmp.link = here;
  return (jmp);
#endif
}


void resolve_forward_jump (forward_jump jmp)
{
#if TARGET_ARDUINO
  uint8_t link = jmp;
#else
  uint8_t link = jmp.link;
#endif
  while (link)
  {
    uint8_t next = prog [link];
    prog [link] = prog_length;
    link = next;
  }
}


backward_jump declare_backward_jump (void)
{
#if TARGET_ARDUINO
  return (prog_length);
#else
  backward_jump jmp = {prog_length};
  return jmp;
#endif
}


void insert_backward_jump (backward_jump jmp)
{
#if TARGET_ARDUINO
  compile_opcode (jmp);
#else
  compile_opcode (jmp.dest);
#endif
}


void compile_expression (obj expr, bool value_context)
{
  if (get_type (expr) == cons_type)
  {
    obj fn;
    obj args;
    decons (expr, &fn, &args);

    if (get_type (fn) == rom_symbol_type)
    {
      const rom_object *hdr = get_rom_header (fn);
      if (pgm_read_byte_near (&hdr -> is_fexpr))
      {
        built_in_fn f = (built_in_fn) pgm_read_word_near (&hdr -> global_fn);
        stack_push (args);
        f (value_context ? (uint8_t *) "y" : NULL);
        stack_pop (1);
        return;
      }
    }
    uint8_t n = 0;
    while (args)
    {
      obj arg1;
      decons (args, &arg1, &args);
      compile_expression (arg1, true);
      n += 1;
    }
    bool maybe_drop = (fn == obj_APPLY);
    if (get_type (fn) == rom_symbol_type)
      compile_opcode (fn);
    else
    {
      compile_opcode (opCALL);
      compile_constant (fn);
      maybe_drop = true;
    }
    if (fn != obj_APPLY && ! value_context)
      n |= 0x80;
    compile_opcode (n);
    if (maybe_drop && ! value_context)
      compile_opcode (opDROP);
  }
  else if (value_context)
    switch (expr)
    {
    case obj_NIL:
      compile_opcode (opLOAD_NIL);
      break;
    case obj_T:
      compile_opcode (opLOAD_T);
      break;
    case obj_ZERO:
      compile_opcode (opLOAD_ZERO);
      break;
    case obj_ZERO + 1:
      compile_opcode (opLOAD_ONE);
      break;

    default:
      if (expr <= LAST_ROM_OBJECT || get_type (expr) == symbol_type)
        compile_opcode (opLOAD_VAR);
      else
        compile_opcode (opLOAD_LITERAL);
      compile_constant (expr);
      break;
    }
}

void compile_constant (obj o)
{
  for (uint8_t i = 0; i < const_length; i += 1)
    if (constants [i] == o)
    {
      compile_opcode (i);
      return;
    }

  if (const_length == MAX_LITERALS_PER_LAMBDA)
    throw_error (no_mem);
  compile_opcode (const_length);
  constants [const_length++] = o;
}

void compile_opcode (uint8_t op)
{
  if (prog_length == MAX_OPCODES_PER_LAMBDA)
    throw_error (no_mem);
  prog [prog_length++] = op;
}

static void compile_pending_expression (obj expr)
{
  compiler_init ();

  obj body = get_header (expr) -> u.closure_val.lambda_obj;
  obj car, cdr;

  decons (body, &car, &cdr);
  switch (car)
  {
  case obj_T:
    compile_expression (cdr, true);
    break;
  case obj_LAMBDA:
    compile_lambda_body (cdr);
    break;
  }
  compile_opcode (opRETURN);
  if (prog_length > longest_prog)
    longest_prog = prog_length;
  if (const_length > longest_const)
    longest_const = const_length;
  obj c_vec = new_extended_object (array_type, const_length);
  memcpy (wksp_obj_ptr (get_header (c_vec) -> u.array_val) + 1,
          constants, const_length * sizeof (obj));
  get_header (body) -> control = lambda_type;
  get_header (body) -> u.lambda_body.constants = c_vec;
  obj b_vec = new_extended_object (string_type, prog_length);
  memcpy (wksp_byte_ptr (get_header (b_vec) -> u.string_val) + 1,
          prog, prog_length);
  get_header (body) -> u.lambda_body.opcodes = b_vec;
  get_header (expr) -> u.closure_val.environment = obj_NIL;

  compiler_report ();
}


obj compile_top_level (obj expr)
{
  objhdr *p = ((expr >= FIRST_RAM_OBJECT && expr <= LAST_POSSIBLE_OBJECT)
               ? get_header (expr) : NULL);
  objhdr *closure_hdr;
  obj closure;

  if (p)
    FIX_OBJ (p);
  {
    closure = new_object (closure_type, &closure_hdr);
    closure_hdr -> u.closure_val.environment = obj_T;
    closure_hdr -> u.closure_val.lambda_obj = cons (obj_T, expr);

    for (;;)
    {
      obj next_to_compile;
      for (next_to_compile = FIRST_RAM_OBJECT;
           next_to_compile <= last_allocated_object;
           next_to_compile += 1)
      {
        closure_hdr = get_header (next_to_compile);
        if (GET_TYPE (closure_hdr) == closure_type &&
            closure_hdr -> u.closure_val.environment == obj_T)
          break;
      }
      if (next_to_compile > last_allocated_object)
        break;
      compile_pending_expression (next_to_compile);
    }
  }
  if (p)
    RELEASE_OBJ (p);

  prog_obj = constants_obj = obj_NIL;
  return (closure);
}

END_IMPLEMENTATION
