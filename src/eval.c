#include "cons.h"
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
  if (get_type (expr) == cons_type)
  {
    uint16_t argc = internal_len (expr);
    objhdr *p = get_header (new_extended_object (array_type, argc));
    p -> flags |= gc_fixed;
    {
      obj car, cdr;
      decons (expr, &car, &cdr);
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
      obj res = fn_apply (p -> u.array_val);
      p -> flags &= ~ gc_fixed;
      return (res);
    }
  }
  return (expr);
}

obj fn_apply (obj *argv)
{
  return (*argv);
}
