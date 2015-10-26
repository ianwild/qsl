#include <Arduino.h>

extern "C" {
#include "io.h"
}

void msg (char *txt)
{
  Serial.println (txt);
  delay (1000);
}

uint8_t readc (void)
{
  while (! Serial.available ())
    ;
  return (Serial.read ());
}

void printc (uint8_t ch)
{
  Serial.write (ch);
}

void error_helper (char *file, int line, char *msg)
{
  Serial.print (file);
  Serial.print (F ("("));
  Serial.print (line);
  Serial.print (F ("): "));
  Serial.println (msg);
  exit (1);
}

