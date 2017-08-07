#include "arrays.h"
#include "cons.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"

START_IMPLEMENTATION

static obj make_xxx (uint8_t *argc, enum typecode t)
{
  adjust_argc (argc, 1);
  int32_t len = get_int_val (get_arg (0));
  if (len < 0 || len > 255)
    throw_error (no_mem);
  return (new_extended_object (t, len));
}


obj fn_make_string (uint8_t *argc)
{
  return (make_xxx (argc, string_type));
}


obj fn_make_array (uint8_t *argc)
{
  return (make_xxx (argc, array_type));
}


obj fn_length (uint8_t *argc)
{
  adjust_argc (argc, 1);

  uint16_t len = 0;
  obj arg = get_arg (0);

  if (arg == obj_NIL)
    return (obj_ZERO);

  switch (get_type (arg))
  {
  case cons_type:
    len = internal_len (arg);
    break;

  case string_type:
    get_spelling (arg, &len);
    break;

  case array_type:
  case environment_type:
    len = get_header (arg) -> u.array_val [0];
    break;

  default:
    throw_error (bad_type);
  }

  return (create_int (len));
}


obj fn_aref (uint8_t *argc)
{
  adjust_argc (argc, 2);

  obj target = get_arg (1);
  int32_t idx = get_int_val (get_arg (0));

  if (idx < 0)
    throw_error (bad_idx);

  switch (get_type (target))
  {
  case string_type:
  {
    uint16_t len;
    uint8_t *p = get_spelling (target, &len);
    if (idx >= len)
      throw_error (bad_idx);
    return (FIRST_CHAR + p [idx]);
  }

  case array_type:
  case environment_type:
  {
    obj *p = get_header (target) -> u.array_val;
    if (idx >= p [0])
      throw_error (bad_idx);
    return (p [idx + 1]);
  }

  default:
    throw_error (bad_type);
    return (obj_NIL);
  }
}


obj fn_aset (uint8_t *argc)
{
  adjust_argc (argc, 3);

  obj target = get_arg (2);
  int32_t idx = get_int_val (get_arg (1));
  obj new_val = get_arg (0);

  if (idx < 0)
    throw_error (bad_idx);

  switch (get_type (target))
  {
  case string_type:
  {
    if (get_type (new_val) != char_type)
      throw_error (bad_type);

    uint16_t len;
    uint8_t *p = get_spelling (target, &len);
    if (idx >= len)
      throw_error (bad_idx);
    p [idx] = new_val - FIRST_CHAR;
    break;
  }

  case array_type:
  {
    obj *p = get_header (target) -> u.array_val;
    if (idx >= p [0])
      throw_error (bad_idx);
    p [idx + 1] = new_val;
    break;
  }

  default:
    throw_error (bad_type);
  }
  return (new_val);
}


obj fn_char_code (uint8_t *argc)
{
  adjust_argc (argc, 1);
  obj arg = get_arg (0);
  if (get_type (arg) != char_type)
    throw_error (bad_type);
  return (create_int (arg - FIRST_CHAR));
}


obj fn_code_char (uint8_t *argc)
{
  adjust_argc (argc, 1);
  int32_t c = get_int_val (get_arg (0));
  if (c < 0 || c > 255)
    throw_error (bad_idx);
  return (FIRST_CHAR + c);
}

END_IMPLEMENTATION
