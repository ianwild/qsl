#ifndef QSL_OBJ_H
#define QSL_OBJ_H

#include "rom-symbols.h"
#include "frozen-objects.h"
#include "target.h"
#include "types.h"

START_HEADER_FILE

#define obj_NIL               OBJECT_C (0)
#define obj_T                 OBJECT_C (1)
#define obj_QUOTE             OBJECT_C (2)
#define obj_LAMBDA            OBJECT_C (3)
#define obj_APPLY             OBJECT_C (4)

#define FIRST_SMALL_INT       OBJECT_C (0x8000)
#define obj_ZERO              OBJECT_C (0xC000)
#define FIRST_CHAR            OBJECT_C (0x7F00)
#define LAST_POSSIBLE_OBJECT  OBJECT_C (0x7EFF)
#define LAST_ROM_OBJECT       ROM_OBJECT_COUNT
#define FIRST_FROZEN_OBJECT   (LAST_ROM_OBJECT + 1)
#define LAST_FROZEN_OBJECT    (FIRST_FROZEN_OBJECT + FROZEN_OBJECT_COUNT)
#define FIRST_RAM_OBJECT      (LAST_FROZEN_OBJECT + 1)

enum typecode      get_type             (obj o);
objhdr            *get_header           (obj o);
uint8_t           *get_spelling         (obj o, uint16_t *len);
const uint8_t     *get_rom_spelling     (obj o, uint16_t *len);
const rom_object  *get_rom_header       (obj o);
obj                new_object           (enum typecode type, objhdr **hdr);
obj                new_extended_object  (enum typecode type, uint16_t size);
void               compact_string_space (void);

#if WITH_THROW_LOCATION
void               throw_error          (enum errcode e,
                                         const char *file, int line);

#define throw_error(e) throw_error (e, this_file, __LINE__)
#else
void               throw_error          (enum errcode e);
#endif


extern obj last_allocated_object;
extern obj working_root;

obj       fn_gc           (uint8_t *argc);
obj       fn_mem          (uint8_t *argc);

END_HEADER_FILE

#endif // QSL_OBJ_H
