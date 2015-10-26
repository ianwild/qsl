
#include "cons.h"
#include "obj.h"

static objhdr *get_cons_header (obj o)
{
  if (get_type (o) != cons_type)
    throw_error (bad_type);
  return (get_header (o));
}

static obj create_cons (obj car, obj cdr)
{
  obj res = new_object (cons_type);
  objhdr *p = get_header (res);
  p -> u.cons_val.car_cell = car;
  p -> u.cons_val.cdr_cell = cdr;
  return (res);
}

obj fn_car (obj *argv)
{
  if (argv [0] != 1)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  if (cons_cell == obj_NIL)
    return (obj_NIL);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.car_cell);
}

obj fn_cdr (obj *argv)
{
  if (argv [0] != 1)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  if (cons_cell == obj_NIL)
    return (obj_NIL);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.cdr_cell);
}

obj fn_cons (obj *argv)
{
  if (argv [0] != 2)
    throw_error (bad_argc);
  return (create_cons (argv [1], argv [2]));
}

obj fn_list (obj *argv)
{
  uint16_t argc = *argv++;
  if (argc == 0)
    return (obj_NIL);
  obj res = create_cons (*argv++, obj_NIL);
  objhdr *p = get_header (res);
  p -> flags |= gc_fixed;
  objhdr *q = p;
  while (--argc)
  {
    obj cdr = create_cons (*argv++, obj_NIL);
    q -> u.cons_val.cdr_cell = cdr;
    q = get_header (cdr);
  }
  p -> flags &= ~ gc_fixed;
  return (res);
}

obj fn_rplca (obj *argv)
{
  if (argv [0] != 2)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.car_cell = argv [2]);
}

obj fn_rplcd (obj *argv)
{
  if (argv [0] != 2)
    throw_error (bad_argc);
  obj cons_cell = argv [1];
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.cdr_cell = argv [2]);
}

