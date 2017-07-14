#include "compiler.h"
#include "cons.h"
#include "dbg.h"
#include "eval.h"
#include "fexprs.h"
#include "io.h"
#include "obj.h"
#include "stack.h"
#include "symbols.h"
#include "target.h"

static const char PROGMEM this_file [] = __FILE__;

obj current_environment;

static bool save_env (void)
{
  if (current_environment == obj_NIL)
    return (false);
  objhdr *p = get_header (current_environment);
  if (p -> flags & gc_fixed)
    return (false);
  p -> flags |= gc_fixed;
  return (true);
}

obj fn_eval (uint8_t *argc)
{
  switch (*argc)
  {
  case 0:
    return (obj_NIL);

  case 1:
    return (eval_internal (get_arg (0)));

  default:
  {
    adjust_argc (argc, 2);
    bool unprotect = save_env ();
    obj keep_env = current_environment;
    current_environment = get_arg (0);
    obj res = eval_internal (get_arg (1));
    current_environment = keep_env;
    if (unprotect)
      get_header (current_environment) -> flags &= ~gc_fixed;
    return (res);
  }
  }
}

#if NOT_YET_CONVERTED
static obj make_argv (obj args, bool is_fexpr)
{
  if (is_fexpr)
  {
    obj res = new_extended_object (array_type, 2);
    obj *p = get_header (res) -> u.array_val;
    p [1] = args;
    p [2] = current_environment;
    return (res);
  }
  else
  {
    uint16_t argc = internal_len (args);
    obj res = new_extended_object (array_type, argc);
    objhdr *p = get_header (res);

    // protect res across the eval_internal() calls
    p -> flags |= gc_fixed;
    {
      for (uint16_t i = 1; i <= argc; i += 1)
      {
        obj car;
        decons (args, &car, &args);
        car = eval_internal (car);
        p -> u.array_val [i] = car;
      }
    }
    p -> flags &= ~ gc_fixed;

    return (res);
  }
}
#endif

static obj make_lambda_binding (obj params, obj args)
{
  uint16_t len = internal_len (params);
  if (len != internal_len (args))
    throw_error (bad_argc);
  if (len == 0)
    return (obj_NIL);
  obj res = new_extended_object (environment_type, 1 + 2 * len);
  objhdr *p = get_header (res);

  // protect res across the eval_internal() calls
  p -> flags |= gc_fixed;
  {
    uint16_t i = 2;
    while (params)
    {
      obj val;
      decons (args, &val, &args);
      val = eval_internal (val);
      obj *bindings = p -> u.array_val;
      decons (params, &bindings [i], &params);
      bindings [i + 1] = val;
      i += 2;
    }
  }
  p -> flags &= ~ gc_fixed;

  return (res);
}

static obj make_fexpr_binding (obj params, obj args)
{
  if (internal_len (params) != 2)
    throw_error (bad_argc);
  obj res = new_extended_object (environment_type, 1 + 2 * 2);
  obj *p = get_header (res) -> u.array_val;

  decons (params, &p [2], &params);
  p [3] = args;
  decons (params, &p [4], &params);
  p [5] = current_environment;

  return (res);
}

obj apply_internal (obj fn, obj args)
{
  switch (get_type (fn))
  {
#if NOT_YET_CONVERTED
  case rom_symbol_type:
  {
    const rom_object *p = get_rom_header (fn);
    built_in_fn f = (built_in_fn) pgm_read_word_near (&p -> global_fn);
    if (! f)
      throw_error (no_fdefn);
    obj argv = make_argv (args, pgm_read_byte_near (&p -> is_fexpr));
    objhdr *argv_hdr = get_header (argv);
    argv_hdr -> flags |= gc_fixed;
    obj res = f (argv);
    argv_hdr -> flags &= ~ gc_fixed;
    return (res);
  }
#endif

  case symbol_type:
  {
    objhdr *fn_hdr = get_header (fn);
    fn = fn_hdr -> u.symbol_val.global_fn;
    if (! fn)
      throw_error (no_fdefn);
    // fall through to "apply closure"
  }

  case closure_type:
  {
    objhdr *fn_hdr = get_header (fn);
    obj code = fn_hdr -> u.closure_val.lambda_obj;
    obj new_env;
    {
      obj type_sym, params;
      decons (code, &type_sym, &code);
      decons (code, &params, &code);
      if (type_sym == obj_LAMBDA)
        new_env = make_lambda_binding (params, args);
      else
        new_env = make_fexpr_binding (params, args);
    }
    if (new_env)
    {
      objhdr *env_hdr = NULL;
      env_hdr = get_header (new_env);
      env_hdr -> u.array_val [1] = fn_hdr -> u.closure_val.environment;
    }
    else
      new_env = fn_hdr -> u.closure_val.environment;

    bool unprotect = save_env ();
    obj keep_env = current_environment;
    current_environment = new_env;
    obj res = eval_progn (code, obj_NIL);
    current_environment = keep_env;
    if (unprotect)
      get_header (current_environment) -> flags &= ~gc_fixed;

    return (res);
  }

  default:
    return (obj_NIL);
  }
}

obj eval_internal (obj expr)
{
  switch (get_type (expr))
  {
  case cons_type:
  {
    obj car, cdr;
    decons (expr, &car, &cdr);
    if (car == obj_LAMBDA)
    {
      objhdr *p;
      obj res = new_object (closure_type, &p);
      p -> u.closure_val.environment = current_environment;
      p -> u.closure_val.lambda_obj = expr;
      return (res);
    }
    return (apply_internal (car, cdr));
  }

  case symbol_type:
  case rom_symbol_type:
    return (symbol_value (expr));

  default:
    return (expr);
  }
}

#if ! TARGET_ARDUINO
static char *symname (uint8_t opcode)
{
  static char buf [128];
  uint16_t n;
  const uint8_t *p = get_rom_spelling (opcode, &n);
  char *q = buf;
  while (n--)
    *q++ = *p++;
  *q = '\0';
  return (buf);
}
#endif

void interpret_bytecodes (void)
{
  uint8_t *function_base = get_opcodes ();
  uint8_t *current_function = function_base;

  for (;;)
  {
    TRACE (("%4zd:\t", current_function - function_base));
    uint8_t opcode = *current_function++;
    switch (opcode)
    {
    case opDROP:
      TRACE (("DROP\n"));
      stack_pop (1);
      break;

    case opSWAP:
      TRACE (("SWAP\n"));
      {
        obj tos = pop_arg ();
        obj nos = pop_arg ();
        stack_push (tos);
        stack_push (nos);
      }
      break;

    case opDUP:
      TRACE (("DUP\n"));
      stack_push (get_arg (0));
      break;

    case opDUP_IF_NIL:
      TRACE (("DUP_IF_NIL\n"));
      if (get_arg (0) == obj_NIL)
        stack_push (get_arg (0));
      break;

    case opDUP_UNLESS_NIL:
      TRACE (("DUP_UNLESS_NIL\n"));
      if (get_arg (0) != obj_NIL)
        stack_push (get_arg (0));
      break;

    case opJUMP_ALWAYS:
      TRACE (("JUMP_ALWAYS %d\n", *current_function));
      current_function = function_base + *current_function;
      break;

    case opJUMP_IF_NIL:
      TRACE (("JUMP_IF_NIL (%04x) %d\n", get_arg (0), *current_function));
      if (pop_arg () == obj_NIL)
        current_function = function_base + *current_function;
      else
        current_function += 1;
      break;

    case opJUMP_UNLESS_NIL:
      TRACE (("JUMP_UNLESS_NIL (%04x) %d\n", get_arg (0), *current_function));
      if (pop_arg () != obj_NIL)
        current_function = function_base + *current_function;
      else
        current_function += 1;
      break;

    case opLOAD_LITERAL:
      TRACE (("LOAD_LITERAL %04x\n", get_const (*current_function)));
      stack_push (get_const (*current_function++));
      break;

    case opLOAD_NIL:
      TRACE (("LOAD_NIL\n"));
      stack_push (obj_NIL);
      break;

    case opLOAD_T:
      TRACE (("LOAD_T\n"));
      stack_push (obj_T);
      break;

    case opLOAD_ZERO:
      TRACE (("LOAD_ZERO\n"));
      stack_push (obj_ZERO);
      break;

    case opLOAD_ONE:
      TRACE (("LOAD_ONE\n"));
      stack_push (obj_ZERO + 1);
      break;

    case opLOAD_VAR:
      TRACE (("LOAD_VAR %04x\n", get_const (*current_function)));
      stack_push (symbol_value (get_const (*current_function++)));
      break;

    case opSETQ:
      TRACE (("SETQ %04x %04x\n", get_const (*current_function), get_arg (0)));
      set_symbol_value (get_const (*current_function++), pop_arg ());
      break;

    case opSET_FDEFN:
      TRACE (("SET_FDEFN %04x %04x\n", get_arg (1), get_arg (0)));
      {
        obj fn = pop_arg ();
        obj sym = pop_arg ();
        objhdr *p = get_header (sym);
        p -> u.symbol_val.global_fn = fn;
      }
      break;

    case opCREATE_CONTEXT_BLOCK:
      TRACE (("CREATE_CONTEXT_BLOCK %d\n", *current_function));
      {
        uint8_t size = *current_function++;
        obj new_env = new_extended_object (environment_type, 1 + 2 * size);
        objhdr *p = get_header (new_env);
        p -> u.array_val [1] = current_environment;
        stack_push (obj_ZERO);
        stack_push (new_env);
      }
      break;

    case opINSERT_BINDING:
      TRACE (("INSERT_BINDING (%04x[%d]: %04x)\n",
              get_arg (1), get_arg (2) - obj_ZERO, get_arg (0)));
      {
        uint8_t idx = 2 + 2 * get_and_incr_arg (2);
        obj sym = get_const (*current_function++);
        objhdr *p = get_header (get_arg (1));
        p -> u.array_val [idx] = sym;
        p -> u.array_val [idx + 1] = pop_arg ();
      }
      break;

    case opPUSH_CONTEXT:
      TRACE (("PUSH_CONTEXT\n"));
      current_environment = pop_arg ();
      break;

    case opPOP_CONTEXT:
      TRACE (("POP_CONTEXT\n"));
      {
        objhdr *p = get_header (current_environment);
        current_environment = p -> u.array_val [1];
      }
      break;

    case opCREATE_CLOSURE:
      TRACE (("CREATE_CLOSURE %04x (%04x)\n",
              get_arg (0), current_environment));
      if (current_environment)
      {
        objhdr *p;
        obj res = new_object (closure_type, &p);
        objhdr *q = get_header (pop_arg ());
        p -> u.closure_val.environment = current_environment;
        p -> u.closure_val.lambda_obj = q -> u.closure_val.lambda_obj;
        stack_push (res);
      }
      break;

    case opCALL:
      TRACE (("CALL %04x/%d\n", get_const (*current_function), current_function [1]));
      {
        obj fn = get_const (*current_function++);
        uint8_t argc = *current_function++;
        objhdr *p = get_header (fn);
        obj lambda = p -> u.symbol_val.global_fn;
        (void) lambda;
        (void) argc;
        TRACE (("lambda is %04x/%d\n", lambda, argc));
      }

    case opRETURN:
      TRACE (("RETURN\n"));
      return;

    default:
      if (opcode <= LAST_ROM_OBJ)
      {
        TRACE (("call builtin |%s| with %d args\n", symname (opcode), *current_function));
        const rom_object *hdr = get_rom_header (opcode);
        built_in_fn fn = (built_in_fn) pgm_read_word_near (&hdr -> global_fn);
        if (! fn || pgm_read_byte_near (&hdr -> is_fexpr))
          throw_error (no_fdefn);
        uint8_t argc = *current_function++;
        bool void_context = (argc >= 128);
        if (void_context)
          argc -= 128;
        obj res = fn (&argc);
        stack_pop (argc);
        if (! void_context)
          stack_push (res);
      }
      else
      {
        TRACE (("opcode is %u = 0x%04x\n", opcode, opcode));
        throw_error (compiler_error);
      }
      break;
    }
  }
}

obj fn_apply (uint8_t *argc)
{
  if (! *argc)
    return (obj_NIL);

  uint8_t n = *argc - 1;
  obj fn = snip_arg (n);

  if (n)
  {
    obj tos = pop_arg ();
    n -= 1;
    while (get_type (tos) == cons_type)
    {
      obj car;
      decons (tos, &car, &tos);
      stack_push (car);
      n += 1;
    }
  }
  *argc = n;

  switch (get_type (fn))
  {
  case rom_symbol_type:
  {
    const rom_object *hdr = get_rom_header (fn);
    if (! hdr -> is_fexpr && hdr -> global_fn)
      return (hdr -> global_fn (argc));
    break;
  }
  }
  throw_error (no_fdefn);
  return (obj_NIL);
}
