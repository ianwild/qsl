#ifndef QSL_DBG_H
#define QSL_DBG_H

#if TARGET_ARDUINO
#define TRACE(x)
#else
#include <stdio.h>
#define TRACE(x) printf x
#endif

#endif // QSL_DBG_H
