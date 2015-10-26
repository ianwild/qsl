#include "cons.h"
#include "fexprs.h"
#include "eval.h"
#include "obj.h"

static obj eval_progn (obj o, obj res)
{
  while (o != obj_NIL)
  {
    obj car, cdr;
    decons (o, &car, &cdr);
    res = eval_here (car);
    o = cdr;
  }
  return (res);
}

obj fe_cond (obj *argv)
{
  uint16_t argc = *argv++;
  while (argc--)
  {
    obj car, cdr;
    decons (*argv++, &car, &cdr);
    obj res = eval_here (car);
    if (res != obj_NIL)
      return (eval_progn (cdr, res));
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
