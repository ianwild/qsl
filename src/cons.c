#include "cons.h"
#include "dbg.h"
#include "obj.h"
#include "stack.h"

START_EXTERN_C

static const char PROGMEM this_file [] = __FILE__;

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
  if (cons == obj_NIL)
  {
    *car = *cdr = obj_NIL;
    return;
  }
  if (get_type (cons) != cons_type)
  {
    TRACE (("type code is %d\n", get_type (cons)));
    throw_error (bad_type);
  }
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

obj fn_car (uint8_t *argc)
{
  adjust_argc (argc, 1);
  obj cons_cell = get_arg (0);
  if (cons_cell == obj_NIL)
    return (obj_NIL);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.car_cell);
}

obj fn_cdr (uint8_t *argc)
{
  adjust_argc (argc, 1);
  obj cons_cell = get_arg (0);
  if (cons_cell == obj_NIL)
    return (obj_NIL);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.cdr_cell);
}

obj fn_cons (uint8_t *argc)
{
  adjust_argc (argc, 2);
  return (cons (get_arg (1), get_arg (0)));
}

obj fn_list (uint8_t *argc)
{
  obj res = obj_NIL;
  while (*argc)
  {
    *argc -= 1;
    res = cons (pop_arg (), res);
  }
  return (res);
}

obj fn_rplaca (uint8_t *argc)
{
  adjust_argc (argc, 2);
  obj cons_cell = get_arg (1);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.car_cell = get_arg (0));
}

obj fn_rplacd (uint8_t *argc)
{
  adjust_argc (argc, 2);
  obj cons_cell = get_arg (1);
  objhdr *p = get_cons_header (cons_cell);
  return (p -> u.cons_val.cdr_cell = get_arg (0));
}

END_EXTERN_C
