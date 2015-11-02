#include "arrays.h"
#include "cons.h"
#include "integer.h"
#include "obj.h"

static const char PROGMEM this_file [] = __FILE__;

static obj make_xxx (obj args, enum typecode t)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 1)
    throw_error (bad_argc);
  int32_t len = get_int_val (argv [1]);
  if (len < 0 || len > 255)
    throw_error (no_mem);
  return (new_extended_object (t, len));
}


obj fn_make_string (obj args)
{
  return (make_xxx (args, string_type));
}


obj fn_make_array (obj args)
{
  return (make_xxx (args, array_type));
}


obj fn_length (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 1)
    throw_error (bad_argc);

  uint16_t len = 0;

  switch (get_type (argv [1]))
  {
  case cons_type:
    len = internal_len (argv [1]);
    break;

  case symbol_type:
  case string_type:
    get_spelling (argv [1], &len);
    break;

  case array_type:
  case environment_type:
    len = get_header (argv [1]) -> u.array_val [0];
    break;

  case rom_symbol_type:
    get_rom_spelling (argv [1], &len);
    break;

  default:
    throw_error (bad_type);
  }

  return (create_int (len));
}


obj fn_aref (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 2)
    throw_error (bad_argc);

  int32_t idx = get_int_val (argv [2]);
  if (idx < 0)
    throw_error (bad_idx);

  switch (get_type (argv [1]))
  {
  case symbol_type:
  case string_type:
  {
    uint16_t len;
    uint8_t *p = get_spelling (argv [1], &len);
    if (idx >= len)
      throw_error (bad_idx);
    return (FIRST_CHAR + p [idx]);
  }

  case array_type:
  case environment_type:
  {
    obj *p = get_header (argv [1]) -> u.array_val;
    if (idx >= p [0])
      throw_error (bad_idx);
    return (p [idx + 1]);
  }

  case rom_symbol_type:
  {
    uint16_t len;
    const uint8_t *p = get_rom_spelling (argv [1], &len);
    if (idx >= len)
      throw_error (bad_idx);
    return (FIRST_CHAR + pgm_read_byte_near (p + idx));
  }

  default:
    throw_error (bad_type);
    return (args);
  }
}


obj fn_aset (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 3)
    throw_error (bad_argc);

  int32_t idx = get_int_val (argv [2]);
  obj new_val = argv [3];

  if (idx < 0)
    throw_error (bad_idx);

  switch (get_type (argv [1]))
  {
  case string_type:
  {
    if (get_type (new_val) != char_type)
      throw_error (bad_type);

    uint16_t len;
    uint8_t *p = get_spelling (argv [1], &len);
    if (idx >= len)
      throw_error (bad_idx);
    p [idx] = new_val - FIRST_CHAR;
    break;
  }

  case array_type:
  {
    obj *p = get_header (argv [1]) -> u.array_val;
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


obj fn_char_code (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 1)
    throw_error (bad_argc);
  if (get_type (argv [1]) != char_type)
    throw_error (bad_type);
  return (create_int (argv [1] - FIRST_CHAR));
}


obj fn_code_char (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 1)
    throw_error (bad_argc);
  int32_t c = get_int_val (argv [1]);
  if (c < 0 || c > 255)
    throw_error (bad_idx);
  return (FIRST_CHAR + c);
}
