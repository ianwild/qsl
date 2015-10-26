#ifndef QSL_TYPE_H
#define QSL_TYPE_H

#include <stdbool.h>
#include <stdint.h>

enum typecode
{
  unallocated_type,
  closure_type,
  symbol_type,
  cons_type,
  int_type,
  string_type,
  array_type,
  environment_type,

  char_type,
  rom_symbol_type,
};

enum flag_bits
{
  gc_wanted  = 0x01,
  gc_scanned = 0x02,
  gc_fixed   = 0x04,

  fexpr      = 0x80
};

enum errcode
{
  no_error,
  bad_type,
  bad_obj,
  bad_argc,
  div_by_zero,
  no_mem
};


typedef uint16_t obj;
#define OBJECT_C(x) UINT16_C(x)

typedef obj (*built_in_fn) (obj args);


typedef struct
{
  const uint8_t     *name;
  const built_in_fn  global_fn;
  const bool         is_fexpr;
} rom_object;

typedef struct
{
  uint8_t flags;
  uint8_t xtype;

  union
  {
    // switch type in
    // case closure_type:
    struct
    {
      obj environment;
      obj code;
    } closure_val;

    // case symbol_type:
    struct
    {
      uint8_t *spelling;
      obj      global_fn;
    } symbol_val;

    // case cons_type:
    struct
    {
      obj car_cell;
      obj cdr_cell;
    } cons_val;

    // case int_type:
    int32_t int_val;

    // case string_type:
    uint8_t *string_val;

    // case array_type:
    // case environment_type:
    obj  *array_val;
  } u;
} objhdr;

#endif /* QSL_TYPE_H */
