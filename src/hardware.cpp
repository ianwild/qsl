#include <Arduino.h>
#include <avr/sleep.h>

extern "C" {
#include "eval.h"
#include "gc.h"
#include "hardware.h"
#include "integer.h"
#include "obj.h"
}

static uint32_t timeout;
static int32_t last_time;

obj fn_pin (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  uint32_t pin = get_int_val (argv [1]);
  uint8_t val = (argv [2] == obj_NIL) ? LOW : HIGH;
  digitalWrite (pin, val);
  return (argv [2]);
}

obj fn_on_tick (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  timeout = get_int_val (argv [1]);
  tick_action = argv [2];
  return (tick_action);
}

obj fn_on_serial (obj args)
{
  obj *argv = get_header (args) -> u.array_val;
  serial_action = argv [1];
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

obj fn_wait_for_event (obj args)
{
  (void) args;
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

obj fn_do_events (obj args)
{
  (void) args;
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
