#ifndef QSL_TYPE_H
#define QSL_TYPE_H

#include <stdbool.h>
#include <stdint.h>

enum __attribute__ ((packed)) typecode
{
  unallocated_type,
  closure_type,
  lambda_type,
  symbol_type,
  cons_type,
  global_binding_type,
  int_type,
  string_type,
  array_type,
  environment_type,

  char_type,
  rom_symbol_type,
};

enum __attribute__ ((packed)) flag_bits
{
  gc_wanted  = 0x01,
  gc_scanned = 0x02,
  gc_fixed   = 0x04,

  fexpr      = 0x80
};

enum __attribute__ ((packed)) errcode
{
  no_error,
  bad_type,
  bad_obj,
  bad_argc,
  bad_idx,
  div_by_zero,
  no_fdefn,
  no_mem,
  compiler_error
};


typedef uint16_t obj;
#define OBJECT_C(x) UINT16_C(x)

#define MAX_TOKEN 255

typedef obj (*built_in_fn) (uint8_t *argc);


typedef struct __attribute__ ((packed)) rom_object
{
  const uint8_t     *name;
  const built_in_fn  global_fn;
  const bool         is_fexpr;
} rom_object;


typedef struct __attribute__ ((packed)) objhdr
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
      obj lambda_obj;
    } closure_val;

    // case lambda_type:
    struct
    {
      obj opcodes;
      obj constants;
    } lambda_body;

    // case symbol_type:
    struct
    {
      uint8_t *spelling;
      obj      global_fn;
    } symbol_val;

    // case cons_type:
    // case global_binding_type:
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
