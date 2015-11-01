#include "cons.h"
#include "gc.h"
#include "io.h"
#include "obj.h"

static objhdr *get_cons_header (obj o)
{
  if (get_type (o) != cons_type)
    throw_error (bad_type);
  return (get_header (o));
}

obj cons (obj car, obj cdr)
{
  objhdr *p;
  obj res = new_object (cons_type, &p);
  p -> u.cons_val.car_cell = car;
  p -> u.cons_val.cdr_cell = cdr;
  return (res);
}

void decons (obj cons, obj *car, obj *cdr)
{
  if (get_type (cons) != cons_type)
    throw_error (bad_type);
  objhdr *p = get_header (cons);
  *car = p -> u.cons_val.car_cell;
  *cdr = p -> u.cons_val.cdr_cell;
}

uint16_t internal_len (obj o)
{
  uint16_t res = 0;
  while (o != obj_NIL)
  {
    obj dummy;
    decons (o, &dummy, &o);
    res += 1;
  }
  return (res);
}

obj fn_car (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 1)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  if (cons_cell == obj_NIL)
    return (obj_NIL);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.car_cell);
}

obj fn_cdr (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 1)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  if (cons_cell == obj_NIL)
    return (obj_NIL);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.cdr_cell);
}

obj fn_cons (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 2)
    throw_error (bad_argc);
  return (cons (argv [1], argv [2]));
}

obj fn_list (obj args)
{
  uint16_t argc = get_header (args) -> u.array_val [0];
  obj res = obj_NIL;
  while (argc)
  {
    res = cons (get_header (args) -> u.array_val [argc], res);
    argc -= 1;
  }
  return (res);
}

obj fn_rplaca (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 2)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.car_cell = argv [2]);
}

obj fn_rplacd (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  if (argv [0] != 2)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.cdr_cell = argv [2]);
}

