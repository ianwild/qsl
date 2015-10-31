#include <Arduino.h>

extern "C" {
#include "hardware.h"
#include "integer.h"
#include "obj.h"
}

obj fn_pin (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  uint32_t pin = get_int_val (argv [1]);
  uint8_t val = (argv [2] == obj_NIL) ? LOW : HIGH;
  digitalWrite (pin, val);
  return (argv [2]);
}
