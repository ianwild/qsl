#include <stdio.h>
#include <stdlib.h>

#include "compiler.h"
#include "cons.h"
#include "io.h"
#include "obj.h"
#include "stack.h"


static uint8_t *prog;
uint8_t prog_length;

static obj *constants;
uint8_t const_length;

void compiler_init (void)
{
  if (! prog)
    prog = malloc (1024 * sizeof (uint8_t));
  if (! constants)
    constants = malloc (1024 * sizeof (obj));
  prog_length = 0;
  const_length = 0;
}

void compiler_report (void)
{
  printf ("Constants:\n");
  unsigned i;
  for (i = 0; i < const_length; i += 1)
  {
    printf ("  %3u: %04x ", i, constants [i]);
    print1 (constants [i]);
    printf ("\n");
  }

  printf ("Opcodes:\n");
  for (i = 0; i < prog_length; i += 1)
    printf (" %02x", prog [i]);
  printf ("\n");
}

obj get_const (uint8_t idx)
{
  return (constants [idx]);
}

uint8_t *get_opcodes (void)
{
  return (prog);
}

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
        f (value_context);
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
    if (get_type (fn) == rom_symbol_type)
      compile_opcode (fn);
    else
    {
      compile_opcode (opCALL);
      compile_constant (fn);
    }
    if (! value_context)
      n |= 0x80;
    compile_opcode (n);
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
  uint8_t i = 0;
  for (i = 0; i < const_length; i += 1)
    if (constants [i] == o)
    {
      prog [prog_length++] = i;
      return;
    }

  prog [prog_length++] = const_length;
  constants [const_length++] = o;
}

void compile_opcode (uint8_t op)
{
  prog [prog_length++] = op;
}

