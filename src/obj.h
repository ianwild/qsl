#ifndef QSL_OBJ_H
#define QSL_OBJ_H

#include "types.h"

#define obj_NIL               OBJECT_C (0)
#define obj_T                 OBJECT_C (1)

#define FIRST_SMALL_INT       OBJECT_C (0x8000)
#define OBJ_ZERO              OBJECT_C (0xC000)
#define FIRST_CHAR            OBJECT_C (0x7F00)
#define LAST_POSSIBLE_OBJECT  OBJECT_C (0x7EFF)


uint8_t            get_flags        (obj o);
uint8_t            get_type         (obj o);
objhdr            *get_header       (obj o);
uint8_t           *get_spelling     (obj o, uint16_t *len);
const uint8_t     *get_rom_spelling (obj o, uint16_t *len);
const rom_object  *get_rom_header   (obj o);
void               throw_error      (enum errcode e, char *file, int line);
#define throw_error(e) throw_error (e, __FILE__, __LINE__)
obj                new_object       (enum typecode type, objhdr **hdr);

extern obj last_allocated_object;

#endif /* QSL_OBJ_H */
