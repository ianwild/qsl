#ifndef QSL_OBJ_H
#define QSL_OBJ_H

#include "types.h"

#define obj_NIL               OBJECT_C (0)
#define obj_T                 OBJECT_C (1)

#define FIRST_SMALL_INT       OBJECT_C (0x8000)
#define OBJ_ZERO              OBJECT_C (0xC000)
#define FIRST_CHAR            OBJECT_C (0x7F00)
#define LAST_POSSIBLE_OBJECT  OBJECT_C (0x7EFF)


objhdr  *get_header    (obj o);
void     error         (enum errcode e);
obj      new_object    (enum typecode type);

extern obj last_allocated_object;

#endif /* QSL_OBJ_H */
