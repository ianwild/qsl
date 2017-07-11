#ifndef QSL_DBG_H
#define QSL_DBG_H

#include <assert.h>

#if TARGET_ARDUINO
#define TRACE(x)
#else
#include <stdio.h>
#define TRACE(x) printf x
#endif

#if ! defined (static_assert)
  #define static_assert _Static_assert
#endif

#endif // QSL_DBG_H
