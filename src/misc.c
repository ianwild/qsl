#include "misc.h"
#include "obj.h"


obj fn_not (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  return (argv [1] == obj_NIL ? obj_T : obj_NIL);
}

obj fn_eq (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  return (argv [1] == argv [2] ? obj_T : obj_NIL);
}

obj fn_neq (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  return (argv [1] != argv [2] ? obj_T : obj_NIL);
}

