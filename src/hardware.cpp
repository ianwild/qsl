#include <Arduino.h>
#include <avr/sleep.h>

#include "announce.h"
#include "gc.h"
#include "hardware.h"
#include "integer.h"
#include "io.h"
#include "obj.h"
#include "stack.h"

static uint32_t timeout;
static int32_t last_time;

static obj tick_action;
static obj serial_action;


void embed_announce (enum announcement ann)
{
  switch (ann)
  {
  case ann_startup:
    init ();
    Serial.begin (9600);
    break;

  case ann_gc_starting:
    want_obj (tick_action);
    want_obj (serial_action);
    break;

  default:
    break;
  }
}


obj fn_pin (uint8_t *argc)
{
  adjust_argc (argc, 2);
  uint32_t pin = get_int_val (get_arg (1));
  uint8_t val = (get_arg (0) == obj_NIL) ? LOW : HIGH;
  digitalWrite (pin, val);
  return (get_arg (1));
}

obj fn_on_tick (uint8_t *argc)
{
  adjust_argc (argc, 2);
  timeout = get_int_val (get_arg (1));
  tick_action = get_arg (0);
  return (tick_action);
}

obj fn_on_serial (uint8_t *argc)
{
  adjust_argc (argc, 1);
  serial_action = get_arg (0);
  return (serial_action);
}

static bool isReady (void)
{
  if (tick_action && millis () - last_time >= timeout)
    return (true);
  if (serial_action && Serial.available ())
    return (true);
  return (false);
}

obj fn_wait_for_event (uint8_t *argc)
{
  (void) argc;
  sleep_enable ();
  set_sleep_mode (SLEEP_MODE_IDLE);
  for (;;)
  {
    noInterrupts ();
    if (isReady ())
      break;
    interrupts ();
    sleep_cpu ();
  }
  interrupts ();
  return (obj_NIL);
}

obj fn_next_event (uint8_t *argc)
{
  (void) argc;

  if (serial_action && Serial.available ())
    return (serial_action);

  if (tick_action && millis () - last_time >= timeout)
  {
    last_time = millis ();
    return (tick_action);
  }

  return (obj_NIL);
}
