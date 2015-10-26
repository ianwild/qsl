#ifndef QSL_NOT_ARDUINO_H
#define QSL_NOT_ARDUINO_H

// some nops for AVR magic

#define PROGMEM
#define pgm_read_byte_near(x) (*(x))
#define pgm_read_word_near(x) (*(x))

#endif /* QSL_NOT_ARDUINO_H */
