#ifndef QSL_HARDWARE_H
#define QSL_HARDWARE_H

#include "target.h"

#if TARGET_ARDUINO

  #include "types.h"

  START_EXTERN_C

  obj  fn_pin               (uint8_t *argc);
  obj  fn_on_tick           (uint8_t *argc);
  obj  fn_on_serial         (uint8_t *argc);
  obj  fn_wait_for_event    (uint8_t *argc);
  obj  fn_next_event        (uint8_t *argc);

  END_EXTERN_C

#endif


#endif // QSL_HARDWARE_H
