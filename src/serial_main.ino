#include <Arduino.h>

extern "C" {
#include "io.h"
#include "obj.h"
}

void setup (void)
{
  Serial.begin (9600);
}


void loop (void)
{
  obj x [2];
  x [0] = 1;
  x [1] = fn_read (NULL);
  built_in_fn f = get_rom_header ((x [1] < 128) ? x [1] : 18) -> global_fn;
  f (x);
  x [1] = new_object (cons_type);
  f (x);
}
