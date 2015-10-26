#include "cons.h"
#include "fexprs.h"
#include "eval.h"
#include "obj.h"

static obj eval_progn (obj o, obj res, obj env)
{
  while (o != obj_NIL)
  {
    obj car, cdr;
    decons (o, &car, &cdr);
    res = eval_internal (car, env);
    o = cdr;
  }
  return (res);
}

obj fe_cond (obj *argv)
{
  obj elist = argv [1];
  obj env = argv [2];
  while (elist != obj_NIL)
  {
    obj clause, car, cdr;
    decons (elist, &clause, &elist);
    decons (clause, &car, &cdr);
    obj res = eval_internal (car, env);
    if (res != obj_NIL)
      return (eval_progn (cdr, res, env));
  }
  return (obj_NIL);
}


obj fe_while (obj *argv)
{
  obj elist = argv [1];
  obj env = argv [2];
  obj car, cdr;
  decons (elist, &car, &cdr);
  while (eval_internal (car, env) != obj_NIL)
    eval_progn (cdr, obj_NIL, env);
  return (obj_NIL);
}

obj fe_quote (obj *argv)
{
  return (argv [1]);
}
