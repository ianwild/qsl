#include "fexprs.h"
#include "eval.h"
#include "obj.h"

static obj cond_test (obj o)
{
  if (o == obj_NIL)
    return (obj_NIL);
  if (get_type (o) != cons_type)
    throw_error (bad_type);
  objhdr *p = get_header (o);
  return (eval_here (get_header (o) -> u.cons_val.car_cell));
}

static obj cond_then (obj o, obj res)
{
  while (o != obj_NIL)
  {
    if (get_type (o) != cons_type)
      throw_error (bad_type);
    objhdr *p = get_header (o);
    res = eval_here (p -> u.cons_val.car_cell);
    o = p -> u.cons_val.cdr_cell;
  }
}

obj fe_cond (obj *argv)
{
  uint16_t argc = *argv++;
  while (argc--)
  {
    obj clause = *argv++;
    obj res = cond_test (clause);
    if (res != obj_NIL)
    {
      res = cond_then (clause, res);
      return (res);
    }
  }
  return (obj_NIL);
}

obj fe_while (obj *argv)
{
  uint16_t argc = *argv++;
  if (argc > 0)
  {
    obj test = *argv;
    while (eval_here (test) != obj_NIL)
    {
      uint16_t i;
      for (i = 1; i <= argc; i += 1)
	eval_here (argv [i]);
    }
  }
  return (obj_NIL);
}

obj fe_quote (obj *argv)
{
  if (*argv != 1)
    throw_error (bad_argc);
  return (argv [1]);
}
