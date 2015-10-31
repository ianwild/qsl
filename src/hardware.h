#ifndef QSL_HARDWARE_H
#define QSL_HARDWARE_H

#if ! USE_LINUX

  #include "types.h"

  obj  fn_pin	(obj args);

#else

  #define fn_pin	NULL

#endif


#endif /* QSL_HARDWARE_H */
