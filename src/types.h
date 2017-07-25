#ifndef QSL_TYPES_H
#define QSL_TYPES_H

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

  last_type_code,
  typecode_mask = 0x0f,

  gc_wanted  = 0x10,
  gc_scanned = 0x20,
  gc_fixed   = 0x40,
  flagbit_mask = 0xf0
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

typedef obj (*built_in_fn) (uint8_t *argc);


typedef struct __attribute__ ((packed)) rom_object
{
  const uint8_t     *name;
  const built_in_fn  global_fn;
  const bool         is_fexpr;
} rom_object;


typedef struct __attribute__ ((packed)) objhdr
{
  uint8_t control;
#define GET_TYPE(p)      ((p) -> control & typecode_mask)
#define GET_FLAGS(p)     ((p) -> control & flagbit_mask)
#define SET_FLAGS(p,f)   ((p) -> control |= (f))
#define CLR_FLAGS(p,f)   ((p) -> control &= ~(f))
#define FIX_OBJ(p)       SET_FLAGS ((p), gc_fixed)
#define RELEASE_OBJ(p)   CLR_FLAGS ((p), gc_fixed)

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

#endif // QSL_TYPES_H
