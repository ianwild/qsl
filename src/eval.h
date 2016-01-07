#ifndef QSL_EVAL_H
#define QSL_EVAL_H

/*

  If eval sees (while (< n 10) (setq n (+ n 1)))
    - since 'while' isn't a true function, 'eval' calls a special handler
    - 'while' then needs to call 'eval' for each clause in the body
    - this nested 'eval' needs a special handler for the 'setq'
    - 'setq' calls *yet another* 'eval' to do the +

  This eval-recursion happens at both the C and Lisp levels.  If we
  were a bit smarter, though, we could flatten the original expression
  to something like:
    XXX: n@, 10, <, jump-if-nil YYY, n@, 1, +, n!, jump XXX, YYY:
  which can be evaluated with *no* recursion.  Yes, we still need the
  original ping-pong to do this compilation, but only at the C level.

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

obj  fn_eval           (uint8_t argc);

obj  eval_internal     (obj expr);
obj  apply_internal    (obj fn, obj args);

void interpret_bytecodes (void);

extern obj current_environment;

END_EXTERN_C

#endif /* QSL_EVAL_H */
