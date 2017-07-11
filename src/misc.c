#include "misc.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"

static const char PROGMEM this_file [] = __FILE__;

obj fn_not (uint8_t *argc)
{
  adjust_argc (argc, 1);
  return (get_arg (0) == obj_NIL ? obj_T : obj_NIL);
}

obj fn_eq (uint8_t *argc)
{
  adjust_argc (argc, 2);
  return (get_arg (0) == get_arg (1) ? obj_T : obj_NIL);
}

obj fn_neq (uint8_t *argc)
{
  adjust_argc (argc, 2);
  return (get_arg (0) != get_arg (1) ? obj_T : obj_NIL);
}


static int32_t compare_two_args (uint8_t *argc)
{
  adjust_argc (argc, 2);
  obj left = get_arg (1);
  obj right = get_arg (0);
  enum typecode t = get_type (left);
  if (t != get_type (right))
    throw_error (bad_type);
  int32_t a =
    (t == char_type) ? (left - FIRST_CHAR) : get_int_val (left);
  int32_t b =
    (t == char_type) ? (right - FIRST_CHAR) : get_int_val (right);

  return (a - b);
}

obj fn_lt (uint8_t *argc)
{
  return ((compare_two_args (argc) < 0) ? obj_T : obj_NIL);
}

obj fn_le (uint8_t *argc)
{
  return ((compare_two_args (argc) <= 0) ? obj_T : obj_NIL);
}

obj fn_gt (uint8_t *argc)
{
  return ((compare_two_args (argc) > 0) ? obj_T : obj_NIL);
}

obj fn_ge (uint8_t *argc)
{
  return ((compare_two_args (argc) >= 0) ? obj_T : obj_NIL);
}

obj fn_equals (uint8_t *argc)
{
  return ((compare_two_args (argc) == 0) ? obj_T : obj_NIL);
}

obj fn_not_equals (uint8_t *argc)
{
  return ((compare_two_args (argc) != 0) ? obj_T : obj_NIL);
}

