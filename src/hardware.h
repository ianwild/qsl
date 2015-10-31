#ifndef QSL_HARDWARE_H
#define QSL_HARDWARE_H

#if ! USE_LINUX

  #include "types.h"

  obj  fn_pin	            (obj args);
  obj  fn_on_tick           (obj args);
  obj  fn_on_serial         (obj args);
  obj  fn_wait_for_event    (obj args);
  obj  fn_do_events         (obj args);
#else

  #define fn_pin		NULL
  #define fn_on_tick		NULL
  #define fn_on_serial		NULL
  #define fn_wait_for_event	NULL
  #define fn_do_events		NULL
#endif


#endif /* QSL_HARDWARE_H */
