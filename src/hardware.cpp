#include <Arduino.h>
#include <avr/sleep.h>

#include "embedded.h"
#include "eval.h"
#include "gc-hooks.h"
#include "hardware.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"

static uint32_t timeout;
static int32_t last_time;

static obj tick_action;
static obj serial_action;

void embed_init (void)
{
  init ();
  Serial.begin (9600);
}

void embed_mark_roots (void)
{
  want_obj (tick_action);
  want_obj (serial_action);
}


obj fn_pin (uint8_t *argc)
{
  adjust_argc (argc, 2);
  uint32_t pin = get_int_val (get_arg (0));
  uint8_t val = (get_arg (1) == obj_NIL) ? LOW : HIGH;
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

static void apply_lambda (obj lambda)
{
  uint8_t argc = 2;
  stack_push (lambda);
  stack_push (obj_NIL);
  fn_apply (&argc);
  stack_pop (argc);
}

obj fn_wait_for_event (uint8_t *argc)
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

obj fn_do_events (uint8_t *argc)
{
  (void) argc;
  uint8_t n = 0;

  if (tick_action && millis () - last_time >= timeout)
  {
    n += 1;
    last_time = millis ();
    apply_lambda (tick_action);
  }

  if (serial_action && Serial.available ())
  {
    n += 1;
    apply_lambda (serial_action);
  }

  return (create_int (n));
}
