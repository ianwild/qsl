#ifndef QSL_BUFFER_LIMITS_H
#define QSL_BUFFER_LIMITS_H

#if TARGET_ARDUINO
  #define TOTAL_HEAP_SIZE        1024
#else
  #define TOTAL_HEAP_SIZE       10240
#endif

#define MAX_OPCODES_PER_LAMBDA     64
#define MAX_LITERALS_PER_LAMBDA    16
#define MAX_STACK_DEPTH            64
#define MAX_TOKEN_LENGTH           32


#endif // QSL_BUFFER_LIMITS_H
