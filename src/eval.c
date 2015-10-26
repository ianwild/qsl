#include "eval.h"
#include "obj.h"

obj current_environment;

obj fn_eval (obj *argv)
{
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
  return (env ? env : expr);
}

obj eval_here (obj expr)
{
  return (eval_internal (expr, current_environment));
}

obj fn_apply (obj *argv)
{
  return (*argv);
}
