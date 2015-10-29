#include "cons.h"
#include "eval.h"
#include "obj.h"
#include "symbols.h"

obj current_environment;

obj fn_eval (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  switch (*argv)
  {
  case 1:
    return (eval_internal (argv [1], obj_NIL));
  case 2:
    return (eval_internal (argv [1], argv [2]));
  default:
    throw_error (bad_argc);
    return (obj_NIL);
  }
}

obj eval_internal (obj expr, obj env)
{
  switch (get_type (expr))
  {
  case cons_type:
  {
    obj car, cdr;
    objhdr *p;
    decons (expr, &car, &cdr);
    if (car == obj_LAMBDA)
    {
      obj res = new_object (closure_type, &p);
      p -> u.closure_val.environment = env;
      p -> u.closure_val.code = expr;
      return (res);
    }
    obj apply_args;
    {
      uint16_t argc = internal_len (expr);
      apply_args = new_extended_object (array_type, argc);
      p = get_header (apply_args);
      p -> flags |= gc_fixed;
      p -> u.array_val [1] = car;
      uint16_t i;
      for (i = 2; i <= argc; i += 1)
      {
	decons (cdr, &car, &cdr);
	car = eval_internal (car, env);
	p -> u.array_val [i] = car;
      }
    }
    {
      obj res = fn_apply (apply_args);
      p -> flags &= ~ gc_fixed;
      return (res);
    }
  }

  case symbol_type:
  case rom_symbol_type:
    return (symbol_value (expr, env));

  default:
    return (expr);
  }
}

obj fn_apply (obj args)
{
  return (args);
}
