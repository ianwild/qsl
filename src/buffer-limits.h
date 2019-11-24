#ifndef QSL_BUFFER_LIMITS_H
#define QSL_BUFFER_LIMITS_H

/*

  TOTAL_HEAP_SIZE
    How much memory to set aside for QSL, including all variables,
    functions, strings, lists, arrays,... plus temporary buffers used
    during reading and compiling expressions.  QSL hasn't been extensively
    tested with a heap smaller than 768 bytes.  However, remember the C
    runtime will need space for variables, a stack, serial buffers,...,
    so don't get _too_ generous.

  MAX_OPCODES_PER_LAMBDA
    Longest allowed compiled function.  Since bytecodes are buffered in
    a string, cannot exceed 255.  Not too critical, since the temporary
    buffer is released after compilation.

  MAX_LITERALS_PER_LAMBDA
    Largest number of objects referenced by a compiled function.  Since
    it takes two bytes to load an object, this cannot usefully exceed
    half of MAX_OPCODES_PER_LAMBDA.  Not too critical, since the temporary
    buffer is released after compilation.

  MAX_STACK_DEPTH
    Deepest allowed stack depth and, therefore, a limit on how deeply
    function calls can be nested.  A function call takes at least three
    levels, plus one per parameter passed.  The stack object itself is
    never released, so any wasted space here is lost forever.

  MAX_TOKEN_LENGTH
    The longest allowed symbol or string literal.  Not too critical, since
    the temporary buffer is released when no longer needed.

*/

#if TARGET_ARDUINO
  #define TOTAL_HEAP_SIZE          1536    // (1024 * 7)
  #define MAX_OPCODES_PER_LAMBDA     64
  #define MAX_LITERALS_PER_LAMBDA    16
  #define MAX_STACK_DEPTH            64
  #define MAX_TOKEN_LENGTH           32
#else
  #define TOTAL_HEAP_SIZE         (512 * 1024)
  #define MAX_OPCODES_PER_LAMBDA    255
  #define MAX_LITERALS_PER_LAMBDA    64
  #define MAX_STACK_DEPTH           256
  #define MAX_TOKEN_LENGTH           32
#endif



#endif // QSL_BUFFER_LIMITS_H
