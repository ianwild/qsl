#include <stdio.h>

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
