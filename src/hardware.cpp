#include <Arduino.h>
#include <avr/sleep.h>

#include "announce.h"
#include "gc.h"
#include "hardware.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"

#if WITH_NAMESPACE
START_IMPLEMENTATION
#endif

static uint32_t timeout;
static int32_t last_time;

static obj tick_action;
static obj serial_action;


void hardware_announce (enum announcement ann)
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

  case ann_shutting_down:
    tick_action = serial_action = obj_NIL;
    break;

  default:
    break;
  }
}


obj fn_pin (uint8_t *argc)
{
  bool just_read = (*argc == 1);
  adjust_argc (argc, 2);
  uint8_t pin = get_int_val (get_arg (1));
  uint8_t old_val = digitalRead (pin);
  if (! just_read)
  {
    uint8_t val = (get_arg (0) == obj_NIL) ? LOW : HIGH;
    digitalWrite (pin, val);
  }
  return (old_val ? obj_T : obj_NIL);
}

obj fn_pwm (uint8_t *argc)
{
  adjust_argc (argc, 2);
  uint8_t pin = get_int_val (get_arg (1));
  uint8_t val = get_int_val (get_arg (0));
  analogWrite (pin, val);
  return (obj_NIL);
}

obj fn_pin_mode (uint8_t *argc)
{
  adjust_argc (argc, 2);
  uint8_t pin = get_int_val (get_arg (1));
  obj arg = get_arg (0);
  uint8_t ch0 = ((get_type (arg) == rom_symbol_type)
                 ? pgm_read_byte_near (get_rom_spelling (arg, NULL))
                 : *get_spelling (arg, NULL));
  switch (ch0)
  {
  case 'i': pinMode (pin, INPUT);         break;
  case 'o': pinMode (pin, OUTPUT);        break;
  case 'p': pinMode (pin, INPUT_PULLUP);  break;
  }
  return (obj_NIL);
}

obj fn_analog_pin (uint8_t *argc)
{
  adjust_argc (argc, 1);
  uint8_t pin = get_int_val (get_arg (1));
  return (create_int (analogRead (pin)));
}

obj fn_millis (uint8_t *argc)
{
  adjust_argc (argc, 0);
  return (create_int (millis ()));
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

#if WITH_PEEK_AND_POKE
obj fn_8bits (uint8_t *argc)
{
  adjust_argc (argc, 2);
  obj val = get_arg (0);
  uint8_t *addr = (uint8_t *) get_int_val (get_arg (1));
  if (val == obj_NIL)
    return (create_int (*addr));
  *addr = get_int_val (val);
  return (val);
}

obj fn_16bits (uint8_t *argc)
{
  adjust_argc (argc, 2);
  obj val = get_arg (0);
  uint16_t *addr = (uint16_t *) get_int_val (get_arg (1));
  if (val == obj_NIL)
    return (create_int (*addr));
  *addr = get_int_val (val);
  return (val);
}
#endif

#if WITH_NAMESPACE
END_IMPLEMENTATION
#endif
