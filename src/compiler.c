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

static obj prog_obj;
static uint8_t *prog;
static uint8_t prog_length;
static uint8_t longest_prog;

static obj constants_obj;
static obj *constants;
static uint8_t const_length;
static uint8_t longest_const;

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
// Okay - this one's a bit of a trick.

// An objhdr is a flag byte followed by one of:
//    some uint16_t objects (so total size is odd); or
//    a pointer (so total size is odd).

// If sizeof(objhdr) is EVEN, then, the compiler has added padding,
// which means it has decided to ignore the packed attribute, which
// probably means it doesn't believe the processor can handle
// misaligned uint16_t or pointer variables.

// Even if it gets far enough to need it, the compact_string_space()
// function is no respecter of alignment, so the first garbage
// collection will cause mysterious failures.  Better to head them off
// early.

static_assert (sizeof (objhdr) & 1, "misaligned accesses might not work");
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
    prog_obj = constants_obj = obj_NIL;

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

void compiler_init (void)
{
  if (! prog_obj)
    prog_obj = new_extended_object (string_type, MAX_OPCODES_PER_LAMBDA);
  if (! constants_obj)
    constants_obj = new_extended_object (array_type, MAX_LITERALS_PER_LAMBDA);
  prog = get_spelling (prog_obj, NULL);
  constants = get_header (constants_obj) -> u.array_val + 1;
  prog_length = 0;
  const_length = 0;
}

#if WITH_COMPILER_STATS
void compiler_report (void)
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
  prog [prog_length++] = jmp;
  return (here);
#else
  prog [prog_length++] = jmp.link;
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
  prog [prog_length++] = jmp;
#else
  prog [prog_length++] = jmp.dest;
#endif
}


void compile_expression (obj expr, bool value_context)
{
  switch (get_type (expr))
  {
  case symbol_type:
  case rom_symbol_type:
    if (value_context)
    {
      switch (expr)
      {
      case obj_NIL:      compile_opcode (opLOAD_NIL);  break;
      case obj_T:        compile_opcode (opLOAD_T);    break;

      default:
        compile_opcode (opLOAD_VAR);
        compile_constant (expr);
        break;
      }
    }
    return;

  case cons_type:
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
    return;
  }

  default:
    if (value_context)
      switch (expr)
      {
      case obj_NIL:      compile_opcode (opLOAD_NIL);  break;
      case obj_T:        compile_opcode (opLOAD_T);    break;
      case obj_ZERO:     compile_opcode (opLOAD_ZERO); break;
      case obj_ZERO + 1: compile_opcode (opLOAD_ONE);  break;

      default:
        compile_opcode (opLOAD_LITERAL);
        compile_constant (expr);
        break;
      }
    return;
  }
}

void compile_constant (obj o)
{
  for (uint8_t i = 0; i < const_length; i += 1)
    if (constants [i] == o)
    {
      prog [prog_length++] = i;
      return;
    }

  prog [prog_length++] = const_length;
  constants [const_length++] = o;
  if (const_length > longest_const)
    longest_const = const_length;
}

void compile_opcode (uint8_t op)
{
  prog [prog_length++] = op;
  if (prog_length > longest_prog)
    longest_prog = prog_length;
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
  obj c_vec = new_extended_object (array_type, const_length);
  memcpy (get_header (c_vec) -> u.array_val + 1,
          constants, const_length * sizeof (obj));
  get_header (body) -> control = lambda_type;
  get_header (body) -> u.lambda_body.constants = c_vec;
  obj b_vec = new_extended_object (string_type, prog_length);
  memcpy (get_header (b_vec) -> u.string_val + 1, prog, prog_length);
  get_header (body) -> u.lambda_body.opcodes = b_vec;
  get_header (expr) -> u.closure_val.environment = obj_NIL;

  compiler_report ();
}


obj compile_top_level (obj expr)
{
  objhdr *p = ((expr > LAST_ROM_OBJ && expr <= LAST_POSSIBLE_OBJECT)
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
      for (next_to_compile = LAST_ROM_OBJ + 1;
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
  return (closure);
}
