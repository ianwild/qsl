#if USE_LINUX
#include "not-arduino.h"
#else
#include <Arduino.h>
#endif

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
    return (eval_internal (argv [1], current_environment));
  case 2:
    return (eval_internal (argv [1], argv [2]));
  default:
    throw_error (bad_argc);
    return (obj_NIL);
  }
}


static obj make_argv (obj args, obj env, bool is_fexpr)
{
  if (is_fexpr)
  {
    obj res = new_extended_object (array_type, 2);
    obj *p = get_header (res) -> u.array_val;
    p [1] = args;
    p [2] = env;
    return (res);
  }
  else
  {
    uint16_t argc = internal_len (args);
    obj res = new_extended_object (array_type, argc);
    objhdr *p = get_header (res);
    p -> flags |= gc_fixed;
    uint16_t i;
    for (i = 1; i <= argc; i += 1)
    {
      obj car;
      decons (args, &car, &args);
      p -> u.array_val [i] = eval_internal (car, env);
    }
    p -> flags &= ~ gc_fixed;
    return (res);
  }
}

static obj apply_internal (obj fn, obj args, obj env)
{
  switch (get_type (fn))
  {
  case rom_symbol_type:
  {
    const rom_object *p = get_rom_header (fn);
    built_in_fn f = (built_in_fn) pgm_read_word_near (&p -> global_fn);
    if (! f)
      throw_error (no_fdefn);
    return (f (make_argv (args, env, p -> is_fexpr)));
  }

  case symbol_type:
  default:
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
    decons (expr, &car, &cdr);
    if (car == obj_LAMBDA)
    {
      objhdr *p;
      obj res = new_object (closure_type, &p);
      p -> u.closure_val.environment = env;
      p -> u.closure_val.code = expr;
      return (res);
    }
    return (apply_internal (car, cdr, env));
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
