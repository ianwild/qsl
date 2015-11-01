#include "cons.h"
#include "fexprs.h"
#include "eval.h"
#include "io.h"
#include "obj.h"
#include "symbols.h"

obj eval_progn (obj o, obj res)
{
  while (o != obj_NIL)
  {
    obj car, cdr;
    decons (o, &car, &cdr);
    res = eval_internal (car);
    o = cdr;
  }
  return (res);
}

static obj split_args (obj args, obj *env)
{
  obj *argv = get_header (args) -> u.array_val;
  *env = argv [2];
  return (argv [1]);
}

obj fe_progn (obj args)
{
  obj env;
  obj elist = split_args (args, &env);
  return (eval_progn (elist, obj_NIL));
}

obj fe_cond (obj args)
{
  obj env;
  obj elist = split_args (args, &env);
  while (elist != obj_NIL)
  {
    obj clause, car, cdr;
    decons (elist, &clause, &elist);
    decons (clause, &car, &cdr);
    obj res = eval_internal (car);
    if (res != obj_NIL)
      return (eval_progn (cdr, res));
  }
  return (obj_NIL);
}


obj fe_while (obj args)
{
  obj env;
  obj elist = split_args (args, &env);
  obj car, cdr;
  decons (elist, &car, &cdr);
  while (eval_internal (car) != obj_NIL)
    eval_progn (cdr, obj_NIL);
  return (obj_NIL);
}

obj fe_quote (obj args)
{
  obj car, cdr;
  decons (get_header (args) -> u.array_val [1], &car, &cdr);
  return (car);
}

obj fe_setq (obj args)
{
  obj env;
  obj arglist = split_args (args, &env);
  obj sym, car, cdr;
  decons (arglist, &sym, &cdr);
  decons (cdr, &car, &cdr);
  if (cdr != obj_NIL)
    throw_error (bad_argc);
  return (set_symbol_value (sym, eval_internal (car)));
}

obj fe_defun (obj args)
{
  obj env;
  obj arglist = split_args (args, &env);
  obj sym, cdr;
  decons (arglist, &sym, &cdr);
  if (get_type (sym) != symbol_type)
    throw_error (bad_type);
  objhdr *p;
  obj closure = new_object (closure_type, &p);

  // protect closure across the cons() call
  p -> flags |= gc_fixed;
  {
    p -> u.closure_val.environment = env;
    p -> u.closure_val.code = cons (obj_LAMBDA, cdr);
  }
  p -> flags &= ~ gc_fixed;

  get_header (sym) -> u.symbol_val.global_fn = closure;
  return (sym);
}

obj fe_and (obj args)
{
  obj res;
  obj elist = split_args (args, &res);
  res = obj_T;
  while (elist && res != obj_NIL)
  {
    obj car;
    decons (elist, &car, &elist);
    res = eval_internal (car);
  }
  return (res);
}

obj fe_or (obj args)
{
  obj res;
  obj elist = split_args (args, &res);
  res = obj_NIL;
  while (elist && res == obj_NIL)
  {
    obj car;
    decons (elist, &car, &elist);
    res = eval_internal (car);
  }
  return (res);
}

static obj let (obj args, bool star)
{
  obj bindings;
  obj elist = split_args (args, &bindings);
  decons (elist, &bindings, &elist);
  uint16_t argc = internal_len (bindings);
  if (argc == 0)
    return (eval_progn (elist, obj_NIL));
  obj new_env = new_extended_object (environment_type, 1 + 2 * argc);
  objhdr *p = get_header (new_env);
  p -> u.array_val [1] = current_environment;
  if (star)
    current_environment = new_env;
  else
    p -> flags |= gc_fixed;
  uint16_t i = 2;
  while (bindings)
  {
    obj one_binding;
    decons (bindings, &one_binding, &bindings);
    if (get_type (one_binding) == cons_type)
    {
      obj var, val;
      decons (one_binding, &var, &one_binding);
      decons (one_binding, &val, &one_binding);
      val = eval_internal (val);
      p -> u.array_val [i] = var;
      p -> u.array_val [i + 1] = val;
    }
    else
      p -> u.array_val [i] = one_binding;
    i += 2;
  }
  if (! star)
  {
    current_environment = new_env;
    p -> flags &= ~gc_fixed;
  }
  obj res = eval_progn (elist, obj_NIL);
  current_environment = p -> u.array_val [1];

  return (res);
}

obj fe_let (obj args)
{
  return (let (args, false));
}

obj fe_let_star (obj args)
{
  return (let (args, true));
}

obj fe_apply (obj args)
{
  obj env;
  obj fn = split_args (args, &env);
  decons (fn, &fn, &args);
  return (apply_internal (eval_internal (fn), args));
}
