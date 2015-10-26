#ifndef QSL_TYPE_H
#define QSL_TYPE_H

#include <stdint.h>

enum typecode
{
  unallocated_type,
  system_fn_type,
  closure_type,
  symbol_type,
  cons_type,
  int_type,
  char_type,
  string_type,
  array_type,
  environment_type
};

enum flag_bits
{
  gc_wanted = 0x01,
  gc_swept  = 0x02,
  gc_fixed  = 0x04,

  fexpr  = 0x80
};

enum errcode
{
  no_error,
  bad_type,
  bad_argc,
  div_by_zero
};


typedef uint16_t obj;
#define OBJECT_C(x) UINT16_C(x)

typedef struct
{
  obj      owner;
  uint16_t length;
  uint8_t  body [1];
} byte_object;

typedef struct
{
  obj      owner;
  uint16_t length;
  obj      body [1];
} word_object;

typedef obj (*built_in_fn) (obj *args);

typedef struct
{
  built_in_fn     fn;
  byte_object name;
} fn_descriptor;


typedef struct
{
  uint8_t flags;
  uint8_t type;

  union
  {
    // switch type in
    // case system_fn_type:
    fn_descriptor *system_fn_val;

    // case closure_type:
    struct
    {
      obj environment;
      obj code;
    } closure_val;

    // case symbol_type:
    struct
    {
      byte_object *spelling;
      obj          global_fn;
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
    struct
    {
      byte_object *spelling;
    } string_val;

    // case array_type:
    // case environment_type:
    struct
    {
      word_object *body;
    } array_val;
  } u;
} objhdr;

#endif /* QSL_TYPE_H */
