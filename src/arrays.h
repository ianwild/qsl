#ifndef QSL_ARRAYS_H
#define QSL_ARRAYS_H

#include "target.h"
#include "types.h"

START_EXTERN_C

obj   fn_make_string	(obj args);
obj   fn_make_array	(obj args);
obj   fn_length		(obj args);
obj   fn_aref		(obj args);
obj   fn_aset		(obj args);
obj   fn_char_code	(obj args);
obj   fn_code_char	(obj args);

END_EXTERN_C

#endif /* QSL_ARRAYS_H */
