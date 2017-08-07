#include <stdint.h>

#include "announce.h"
#include "dbg.h"

START_IMPLEMENTATION

void announce (enum announcement ann)
{
  typedef void (*announcement_listener) (enum announcement ann);

  static const PROGMEM announcement_listener listener [] = {
#if TARGET_ARDUINO
    hardware_announce,
#endif
    obj_announce,
    compiler_announce,
    eval_announce,
    io_announce,
    stack_announce
  };
  const int8_t listener_count = (sizeof (listener) / sizeof (*listener));


  if (ann <= ann_gc_starting)
    for (int8_t i = 0; i < listener_count; i += 1)
    {
      const announcement_listener a =
        (announcement_listener) pgm_read_word_near (listener + i);
      a (ann);
    }
  else
    for (int8_t i = listener_count - 1; i >= 0; i -= 1)
    {
      const announcement_listener a =
        (announcement_listener) pgm_read_word_near (listener + i);
      a (ann);
    }
}

END_IMPLEMENTATION
