#include "serial_io.h"

#if WITH_NAMESPACE
START_EXTERN_C
#endif

uint8_t serial_readc  (void) __attribute__ ((weak));
int16_t serial_peekc  (void) __attribute__ ((weak));
void    serial_printc (void) __attribute__ ((weak));

uint8_t serial_readc (void)
{
  while (! Serial.available ())
    ;

  return (Serial.read ());
}

int16_t serial_peekc (void)
{
  if (Serial.available ())
    return (Serial.read ());
  return (-1);
}

void serial_printc (uint8_t ch)
{
  Serial.write (ch);
  if (slow_output)
    delay (10);
}

#if WITH_NAMESPACE
END_EXTERN_C
#endif
