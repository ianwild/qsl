#include "misc.h"
#include "integer.h"
#include "obj.h"

static const char PROGMEM this_file [] = __FILE__;

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


static int32_t compare_two_args (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  uint16_t argc = argv [0];
  if (argc != 2)
    throw_error (bad_argc);
  enum typecode t = get_type (argv [1]);
  if (t != get_type (argv [2]))
    throw_error (bad_type);
  int32_t a =
    (t == char_type) ? (argv [1] - FIRST_CHAR) : get_int_val (argv [1]);
  int32_t b =
    (t == char_type) ? (argv [2] - FIRST_CHAR) : get_int_val (argv [2]);

  return (a - b);
}

obj fn_lt (obj args)
{
  return ((compare_two_args (args) < 0) ? obj_T : obj_NIL);
}

obj fn_le (obj args)
{
  return ((compare_two_args (args) <= 0) ? obj_T : obj_NIL);
}

obj fn_gt (obj args)
{
  return ((compare_two_args (args) > 0) ? obj_T : obj_NIL);
}

obj fn_ge (obj args)
{
  return ((compare_two_args (args) >= 0) ? obj_T : obj_NIL);
}

obj fn_equals (obj args)
{
  return ((compare_two_args (args) == 0) ? obj_T : obj_NIL);
}

obj fn_not_equals (obj args)
{
  return ((compare_two_args (args) != 0) ? obj_T : obj_NIL);
}

