#ifndef QSL_HARDWARE_H
#define QSL_HARDWARE_H

#include "target.h"

#if TARGET_ARDUINO

  #include "types.h"

  START_HEADER_FILE

  obj  fn_pin               (uint8_t *argc);
  obj  fn_pwm               (uint8_t *argc);
  obj  fn_pin_mode          (uint8_t *argc);
  obj  fn_analog_pin        (uint8_t *argc);
  obj  fn_on_tick           (uint8_t *argc);
  obj  fn_on_serial         (uint8_t *argc);
  obj  fn_wait_for_event    (uint8_t *argc);
  obj  fn_next_event        (uint8_t *argc);

  END_HEADER_FILE

#endif // TARGET_ARDUINO


#endif // QSL_HARDWARE_H
