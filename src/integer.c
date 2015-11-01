#include "integer.h"
#include "io.h"
#include "obj.h"

int32_t get_int_val (obj o)
{
  if (o >= FIRST_SMALL_INT)
    return ((int32_t) o - (int32_t) OBJ_ZERO);
  if (get_type (o) != int_type)
    throw_error (bad_type);
  return (get_header (o) -> u.int_val);
}

obj create_int (int32_t val)
{
  uint32_t xval = (uint32_t) val + OBJ_ZERO;
  if (xval >= FIRST_SMALL_INT && xval <= UINT16_MAX)
    return ((obj) xval);
  objhdr *p;
  obj res = new_object (int_type, &p);
  p -> u.int_val = val;
  return (res);
}

obj fn_plus (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  uint16_t argc = *argv++;
  int32_t sum = 0;
  while (argc--)
    sum += get_int_val (*argv++);
  return (create_int (sum));
}

obj fn_times (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  uint16_t argc = *argv++;
  int32_t prod = 1;
  while (argc--)
    prod *= get_int_val (*argv++);
  return (create_int (prod));
}

obj fn_minus (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  uint16_t argc = *argv++;
  if (argc == 0)
    throw_error (bad_argc);
  int32_t ans = get_int_val (*argv++);
  if (argc == 1)
    return (create_int (- ans));
  while (--argc)
    ans -= get_int_val (*argv++);
  return (create_int (ans));
}

obj fn_divide (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  uint16_t argc = *argv++;
  if (argc == 0)
    throw_error (bad_argc);
  int32_t ans = get_int_val (*argv++);
  if (argc == 1)
  {
    if (ans == 0)
      throw_error (div_by_zero);
    return (create_int (1 / ans));
  }
  while (--argc)
  {
    int32_t factor = get_int_val (*argv++);
    if (factor == 0)
      throw_error (div_by_zero);
    ans /= factor;
  }
  return (create_int (ans));
}

