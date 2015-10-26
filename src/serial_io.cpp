#include <Arduino.h>

extern "C" {
#include "io.h"
}

uint8_t readc (void)
{
  while (! Serial.available ())
    ;
  return (Serial.read ());
}

void printc (uint8_t ch)
{
  if (ch == '\n')
    Serial.write ('\r');
  Serial.write (ch);
}

