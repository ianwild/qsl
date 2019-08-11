#include <string.h>

#include "misc.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"

START_IMPLEMENTATION

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

static const uint8_t *find_string_addr (obj o, uint16_t *len, bool *in_rom)
{
  #if FROZEN_OBJECT_COUNT
  if (o >= FIRST_FROZEN_OBJECT && o <= LAST_FROZEN_OBJECT)
  {
    const uint8_t *s = get_frozen_spelling (o);
    *len = pgm_read_byte_near (s++);
    *in_rom = true;
    return (s);
  }
  #endif
  *in_rom = false;
  return (get_spelling (o, len));
}

static int32_t compare_two_args (uint8_t *argc)
{
  adjust_argc (argc, 2);
  obj left = get_arg (1);
  obj right = get_arg (0);
  enum typecode t = get_type (left);
  if (t != get_type (right))
    throw_error (bad_type);
  if (t == string_type)
  {
    uint16_t llen;
    bool lrom;
    const uint8_t *s1 = find_string_addr (left, &llen, &lrom);
    uint16_t rlen;
    bool rrom;
    const uint8_t *s2 = find_string_addr (right, &rlen, &rrom);
    return (compare_strings (s1, llen, lrom, s2, rlen, rrom));
  }
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


int compare_strings (const uint8_t *s1, uint16_t len1, bool in_rom1,
                     const uint8_t *s2, uint16_t len2, bool in_rom2)
{
  size_t n = (len1 < len2) ? len1 : len2;
  for (size_t i = 0; i < n; i += 1)
  {
    uint8_t ch1 = in_rom1 ? pgm_read_byte_near (s1 + i) : s1 [i];
    uint8_t ch2 = in_rom2 ? pgm_read_byte_near (s2 + i) : s2 [i];
    int delta = ch1 - ch2;
    if (delta)
      return (delta);
  }
  return (len1 - len2);
}

END_IMPLEMENTATION
