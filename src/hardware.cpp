#include <Arduino.h>
#include <avr/sleep.h>

#include "eval.h"
#include "gc.h"
#include "hardware.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"

static uint32_t timeout;
static int32_t last_time;

obj fn_pin (uint8_t argc)
{
  uint32_t pin = get_int_val (get_arg (0));
  uint8_t val = (get_arg (1) == obj_NIL) ? LOW : HIGH;
  digitalWrite (pin, val);
  return (get_arg (1));
}

obj fn_on_tick (uint8_t argc)
{
  timeout = get_int_val (get_arg (1));
  tick_action = get_arg (0);
  return (tick_action);
}

obj fn_on_serial (uint8_t argc)
{
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

obj fn_wait_for_event (uint8_t argc)
{
  (void) argc;
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

obj fn_do_events (uint8_t argc)
{
  (void) argc;
  uint8_t n = 0;

  if (tick_action && millis () - last_time >= timeout)
  {
    n += 1;
    last_time = millis ();
    apply_internal (tick_action, obj_NIL);
  }

  if (serial_action && Serial.available ())
  {
    n += 1;
    apply_internal (serial_action, obj_NIL);
  }

  return (create_int (n));
}
