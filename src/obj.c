#include <string.h>

#include "announce.h"
#include "arrays.h"
#include "buffer-limits.h"
#include "compiler.h"
#include "cons.h"
#include "dbg.h"
#include "eval.h"
#include "fexprs.h"
#include "gc.h"
#include "hardware.h"
#include "integer.h"
#include "misc.h"
#include "io.h"
#include "obj.h"
#include "stack.h"

START_IMPLEMENTATION

static_assert (TOTAL_HEAP_SIZE >= 768, "heap too small");

static uint8_t string_space [TOTAL_HEAP_SIZE];
static uint8_t *string_space_top = string_space;
static objhdr *const headers =
  (objhdr *) (string_space + sizeof (string_space));

obj last_allocated_object = FIRST_RAM_OBJECT - 1;

/*
  Working memory looks like this:

     headers                            -> ---------  high memory
                                           |       |
     get_header(last_allocated_object)  -> |~ v v ~|
                                           |       |
                                           |       |  (gap)
                                           |       |
     string_space_top                   -> |~ ^ ^ ~|
                                           |       |
     string_space                       -> ---------  low memory

  Every real object has a header in the `headers` array.  The
  `headers` array is negatively indexed, and grows downwards through
  memory.  Extended objects (strings, arrays, and their close
  relatives) also have a "body" in the low-memory section, which grows
  upwards.  Free memory is the gap between the two.

  The garbage collector can compact the `string_space` area, but
  doesn't move the `headers` contents, except that unused elements
  might get disappeared.

*/

obj working_root;

#if USE_DIRECT_POINTERS

#define WKSP_OBJ_IDX(x) ((obj *) (x))
#define WKSP_BYTE_IDX(x) ((uint8_t *) (x))

#else

#define WKSP_OBJ_IDX(x) ((uint16_t) ((x) - string_space))
#define WKSP_BYTE_IDX(x) ((uint16_t) ((x) - string_space))

uint8_t *wksp_byte_ptr (uint16_t x)
{
  return (string_space + x);
}

obj *wksp_obj_ptr (uint16_t x)
{
  return ((obj *) (string_space + x));
}
#endif

#if TARGET_ARDUINO
  #define ARDUINO_FN(fn) fn
#else
  #define ARDUINO_FN(fn) NULL
#endif
#include "rom-symbols.ci"

#if FROZEN_OBJECT_COUNT > 0
  #include "frozen-objects.ci"
#endif

#if WITH_MEMSTATS
static void memstats (bool gc_done)
{
  uint8_t *ht = (uint8_t*) headers;
  uint8_t *hb = (uint8_t *) get_header (last_allocated_object);
  printc ('[');
  print_int (string_space_top - string_space);
  printc ('|');
  print_int (hb - string_space_top);
  printc ('|');
  print_int (ht - hb);
  printc (']');
  if (gc_done)
    printc ('\n');
  else
    printc ('>');
}
#else
#define memstats(b)
#endif

obj fn_gc (uint8_t *argc)
{
  (void) argc;
  uint8_t *h = (uint8_t *) get_header (last_allocated_object);
  int32_t before = h - string_space_top;
  do_gc ();
  h = (uint8_t *) get_header (last_allocated_object);
  int32_t after = h - string_space_top;
  return (create_int (after - before));
}

obj fn_mem (uint8_t *argc)
{
  adjust_argc (argc, 1);
  int32_t n;
  switch (get_arg (0))
  {
  case obj_ZERO + 0:
    n = string_space_top - string_space;
    break;

  case obj_ZERO + 1:
    n = (uint8_t *) get_header (last_allocated_object) - string_space_top;
    break;

  case obj_ZERO + 2:
    n = last_allocated_object - FIRST_RAM_OBJECT;
    break;

  case obj_ZERO + 10 + 0:
    n = get_stack_deepest ();
    break;

  case obj_ZERO + 20 + 0:
    n = get_longest_opcodes ();
    break;

  case obj_ZERO + 20 + 1:
    n = get_longest_constants ();
    break;

  default:
    return (obj_NIL);
  }
  return (create_int (n));
}

static objhdr *GET_HEADER (obj o)
{
  // just like get_header(), but with less checking
  return (headers - (o - LAST_FROZEN_OBJECT));
}

objhdr *get_header (obj o)
{
  if (o < FIRST_RAM_OBJECT || o > last_allocated_object)
    throw_error (bad_obj);

  return (GET_HEADER (o));
}

const rom_object *get_rom_header (obj o)
{
  if (o > LAST_ROM_OBJECT)
    throw_error (bad_obj);

  return (rom_symbols + o);
}

uint8_t *get_spelling (obj o, uint16_t *len)
{
  objhdr *hdr = get_header (o);
  uint8_t *p;

  switch (GET_TYPE (hdr))
  {
  case string_type:
    p = wksp_byte_ptr (hdr -> u.string_val);
    break;

  case symbol_type:
    p = wksp_byte_ptr (hdr -> u.symbol_val.spelling);
    break;

  default:
    throw_error (bad_obj);
    p = NULL;
  }
  if (len)
    *len = *p;
  return (p + 1);
}

const uint8_t *get_rom_spelling (obj o, uint16_t *len)
{
  if (o > LAST_ROM_OBJECT)
    throw_error (bad_obj);
  const uint8_t *p = (uint8_t *)pgm_read_word_near (&rom_symbols [o].name);
  *len = pgm_read_byte_near (p);
  return (p + 1);
}

static obj check_available_space (int16_t string_space_needed)
{
  bool once = false;
  for (;;)
  {
    objhdr *p = headers - 1;
    obj i = FIRST_RAM_OBJECT;
    while (i <= last_allocated_object)
    {
      if (GET_TYPE (p) == unallocated_type)
        break;
      i += 1;
      p -= 1;
    }
    int16_t really_needed = string_space_needed;
    obj new_last = last_allocated_object;
    if (i > last_allocated_object)
    {
      really_needed += sizeof (*p);
      new_last += 1;
    }
    if (string_space_top + really_needed < (uint8_t *) GET_HEADER (new_last))
    {
      last_allocated_object = new_last;
      return (i);
    }
    if (once)
      throw_error (no_mem);
    once = true;
    do_gc ();
  }
}

obj new_object (enum typecode type, objhdr **hdr)
{
  obj res = check_available_space (0);
  objhdr *p = GET_HEADER (res);
  *p = (objhdr) {.control = type, .u = {}};
  if (hdr)
    *hdr = p;
  return (working_root = res);
}

obj new_extended_object (enum typecode type, uint16_t size)
{
  int16_t true_size = size + 1;

  switch (type)
  {
  case string_type:
  case symbol_type:
    break;
  case array_type:
  case environment_type:
    true_size *= sizeof (obj);
    break;
  default:
    throw_error (bad_type);
  }

  obj res = check_available_space (true_size);
  objhdr *p = GET_HEADER (res);
  *p = (objhdr) {.control = type, .u = {}};
  *(obj *) string_space_top = res;
  string_space_top += sizeof (obj);
  switch (type)
  {
  case string_type:
    p -> u.string_val = WKSP_BYTE_IDX (string_space_top);
    *string_space_top = size;
    string_space_top += size + 1;
    break;
  case symbol_type:
    p -> u.symbol_val.spelling = WKSP_BYTE_IDX (string_space_top);
    *string_space_top = size;
    string_space_top += size + 1;
    break;
  case array_type:
  case environment_type:
    p -> u.array_val = WKSP_OBJ_IDX (string_space_top);
    *(obj *) string_space_top = (obj) size;
    string_space_top += sizeof (obj);
    memset (string_space_top, 0, size * sizeof (obj));
    string_space_top += size * sizeof (obj);
    break;
  default:
    break;
  }
  return (working_root = res);
}

enum typecode get_type (obj o)
{
  if (o >= FIRST_SMALL_INT)
    return (int_type);
  if (o >= FIRST_CHAR)
    return (char_type);
  if (o <= LAST_ROM_OBJECT)
    return (rom_symbol_type);
  #if FROZEN_OBJECT_COUNT
  if (o >= FIRST_FROZEN_OBJECT && o <= LAST_FROZEN_OBJECT)
  {
    const frozen_hdr *p = get_frozen_header (o);
    return (pgm_read_byte_near (&p -> typecode));
  }
  #endif
  if (o > last_allocated_object)
    return (unallocated_type);
  return (GET_TYPE (GET_HEADER (o)));
}

void compact_string_space (void)
{
  uint8_t *from = string_space;
  uint8_t *to = string_space;

  while (from < string_space_top)
  {
    obj o = *(obj *)from;
    objhdr *p = get_header (o);
    uint16_t len = 0;
    void *back_ptr = NULL;
    switch (GET_TYPE (p))
    {
    case symbol_type:
    case string_type:
      len = * (from + sizeof (obj)) + 1;
      back_ptr = wksp_byte_ptr (p -> u.string_val);
      break;
    case array_type:
    case environment_type:
      len = * (obj *) (from + sizeof (obj)) + 1;
      len *= sizeof (obj);
      back_ptr = wksp_obj_ptr (p -> u.array_val);
      break;
    default:
      break;
    }
    len += sizeof (obj);
    if ((GET_FLAGS (p) & gc_wanted) && (back_ptr == from + sizeof (obj)))
    {
      if (to != from)
        memmove (to, from, len);
      p -> u.string_val = WKSP_BYTE_IDX (to + sizeof (obj));
      to += len;
    }
    from += len;
  }

  string_space_top = to;
}

void obj_announce (enum announcement ann)
{
  switch (ann)
  {
  case ann_computation_aborted:
  case ann_shutting_down:

    // All bets are off, so
    //     (a) the "current object" is no longer sacrosanct
    working_root = obj_NIL;

    for (obj i = FIRST_RAM_OBJECT; i <= last_allocated_object; i += 1)
    {
      objhdr *p = GET_HEADER (i);
      if (GET_TYPE (p) == closure_type &&
          p -> u.closure_val.environment == obj_T)
      {
        // (b) the compiler's "pending expressions" are removed; and
        p -> u.closure_val.environment = obj_NIL;
      }

      //   (c) anything temporarily marked as `gc_fixed` is RELEASEd
      RELEASE_OBJ (p);
    }
    break;

  case ann_gc_starting:
    memstats (false);
    want_obj (working_root);
    break;

  case ann_gc_finished:
    memstats (true);
    break;

  default:
    break;
  }
}

END_IMPLEMENTATION
