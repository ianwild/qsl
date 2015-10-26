#include "integer.h"
#include "obj.h"

static int32_t get_int_val (obj o)
{
  if (o >= FIRST_SMALL_INT)
    return ((int32_t) o - (int32_t) OBJ_ZERO);
  objhdr *p = get_header (o);
  if (p -> type != int_type)
    error (bad_type);
  return (p -> u.int_val);
}

static obj create_int (int32_t val)
{
  uint32_t xval = (uint32_t) val + OBJ_ZERO;
  if (xval >= FIRST_SMALL_INT && xval <= UINT16_MAX)
    return ((obj) xval);
  obj res = new_object (int_type);
  objhdr *p = get_header (res);
  p -> u.int_val = val;
  return (res);
}

obj fn_plus (obj *args)
{
  uint16_t argc = *args++;
  int32_t sum = 0;
  while (argc--)
    sum += get_int_val (*args++);
  return (create_int (sum));
}

obj fn_times (obj *args)
{
  uint16_t argc = *args++;
  int32_t prod = 1;
  while (argc--)
    prod *= get_int_val (*args++);
  return (create_int (prod));
}

obj fn_minus (obj *args)
{
  uint16_t argc = *args++;
  if (argc == 0)
    error (bad_argc);
  int32_t ans = *args++;
  if (argc == 1)
    return (create_int (- ans));
  while (--argc)
    ans -= get_int_val (*args++);
  return (create_int (ans));
}

obj fn_divide (obj *args)
{
  uint16_t argc = *args++;
  if (argc == 0)
    error (bad_argc);
  int32_t ans = *args++;
  if (argc == 1)
  {
    if (ans == 0)
      error (div_by_zero);
    return (create_int (1 / ans));
  }
  while (--argc)
  {
    int32_t factor = get_int_val (*args++);
    if (factor == 0)
      error (div_by_zero);
    ans /= factor;
  }
  return (create_int (ans));
}

