#ifndef QSL_EVAL_H
#define QSL_EVAL_H

/*

  Problem: what happens to any nested (lambda ...) expressions during
  compilation?

  How about ... An inner "compiled lambda" is only a skeleton - it
  needs to be combined with an environment before it becomes runnable.
  So, we can create a sort of "bookmark", a "compiled lambda" object
  whose "body" field is the (nested) list to be compiled and whose
  "environment" field is a marker saying "not yet compiled".  Then,
  once the outer clause has been compiled, we go back and look for
  these bookmarks.  That way we can compile arbitrarily nested lambdas
  without recursing in the compiler.

*/


#include "target.h"
#include "types.h"

START_EXTERN_C

obj    fn_apply             (uint8_t *argc);

obj    interpret_top_level  (obj closure);
void   restore_eval_state   (void);

extern obj current_environment;

END_EXTERN_C

#endif // QSL_EVAL_H
