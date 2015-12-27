#include "cons.h"
#include "eval.h"
#include "fexprs.h"
#include "io.h"
#include "obj.h"
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

obj fn_eval (uint8_t argc)
{
  switch (argc)
  {
  case 1:
    return (eval_internal (get_arg (0)));
  case 2:
  {
    bool unprotect = save_env ();
    obj keep_env = current_environment;
    current_environment = get_arg (0);
    obj res = eval_internal (get_arg (1));
    current_environment = keep_env;
    if (unprotect)
      get_header (current_environment) -> flags &= ~gc_fixed;
    return (res);
  }
  default:
    throw_error (bad_argc);
    return (obj_NIL);
  }
}


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
      uint16_t i;
      for (i = 1; i <= argc; i += 1)
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
    obj code = fn_hdr -> u.closure_val.code;
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
      p -> u.closure_val.code = expr;
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

static obj *const_base;
static obj get_const (uint8_t idx)
{
  const_base [idx];
}

static void interpret_bytecodes (void)
{
  uint8_t *current_function;

  for (;;)
  {
    uint8_t opcode = *current_function++;
    switch (opcode)
    {
    case opDUP:
      stack_push (get_arg (0));
      break;

    case opJUMP_FORWARD_ALWAYS:
      current_function += *current_function + 1;
      break;

    case opJUMP_FORWARD_IF_NIL:
      if (pop_arg () == obj_NIL)
	current_function += *current_function + 1;
      else
	current_function += 1;
      break;

    case opJUMP_FORWARD_UNLESS_NIL:
      if (pop_arg () != obj_NIL)
	current_function += *current_function + 1;
      else
	current_function += 1;
      break;

    case opJUMP_BACKWARD_ALWAYS:
      current_function -= *current_function + 1;
      break;

    case opJUMP_BACKWARD_IF_NIL:
      if (pop_arg () == obj_NIL)
	current_function -= *current_function + 1;
      else
	current_function += 1;
      break;

    case opJUMP_BACKWARD_UNLESS_NIL:
      if (pop_arg () != obj_NIL)
	current_function -= *current_function + 1;
      else
	current_function += 1;
      break;

    case opLOAD_LITERAL
      stack_push (get_const (*current_function++));
      break;

    case opLOAD_VAR:
      stack_push (symbol_value (get_const (*current_function++)));
      break;

    case opSETQ:
      set_symbol_value (get_const (*current_function++), pop_arg ());
      break;

    default:
      if (opcode <= LAST_ROM_OBJ)
      {
	const rom_object *hdr = get_rom_header (opcode);
	built_in_fn fn = (built_in_fn) pgm_read_word_near (&hdr -> global_fn);
	if (! fn || pgm_read_byte_near (&hdr -> is_fexpr))
	  throw_error (no_fdefn);
	uint8_t argc = *current_function++;
	bool void_context = (argc >= 128);
	if (void_context)
	  argc -= 128;
	obj res = fn (argc);
	stack_pop (argc);
	if (! void_context)
	  stack_push (res);
      }
      else
	throw_error (compiler_error);
      break;
    }
  }
}

