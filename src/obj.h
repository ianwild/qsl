#ifndef QSL_OBJ_H
#define QSL_OBJ_H

#include "target.h"
#include "types.h"

#define obj_NIL               OBJECT_C (0)
#define obj_T                 OBJECT_C (1)
#define obj_QUOTE             OBJECT_C (2)
#define obj_LAMBDA            OBJECT_C (3)
#define obj_APPLY             OBJECT_C (4)

#define FIRST_SMALL_INT       OBJECT_C (0x8000)
#define obj_ZERO              OBJECT_C (0xC000)
#define FIRST_CHAR            OBJECT_C (0x7F00)
#define LAST_POSSIBLE_OBJECT  OBJECT_C (0x7EFF)

START_EXTERN_C

enum typecode      get_type             (obj o);
objhdr            *get_header           (obj o);
uint8_t           *get_spelling         (obj o, uint16_t *len);
const uint8_t     *get_rom_spelling     (obj o, uint16_t *len);
const rom_object  *get_rom_header       (obj o);
obj                new_object           (enum typecode type, objhdr **hdr);
obj                new_extended_object  (enum typecode type, uint16_t size);
void               compact_string_space (void);

void               throw_error          (enum errcode e,
                                         const char *file, int line);
#define throw_error(e) throw_error (e, this_file, __LINE__)

extern obj last_allocated_object;
extern obj working_root;

obj       fn_gc           (uint8_t *argc);
obj       fn_mem          (uint8_t *argc);

END_EXTERN_C

#endif // QSL_OBJ_H
