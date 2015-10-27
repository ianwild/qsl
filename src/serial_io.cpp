#include <Arduino.h>

extern "C" {
#include "io.h"
}

static int latch = -1;

uint8_t readc (void)
{
  if (latch >= 0)
  {
    int ch = latch;
    latch = -1;
    return (ch);
  }

  while (! Serial.available ())
    ;

  return (Serial.read ());
}

int peekc (void)
{
  if (latch >= 0)
    return (latch);
  if (Serial.available ())
    return (latch = Serial.read ());
  return (-1);
}

void printc (uint8_t ch)
{
  Serial.write (ch);
}

