#ifndef QSL_COMPILER_H
#define QSL_COMPILER_H

#include "target.h"
#include "types.h"

#if TARGET_ARDUINO

typedef uint16_t forward_jump;
typedef uint16_t backward_jump;

#else

typedef struct
{
  int link;
} forward_jump;

typedef struct
{
  int dest;
} backward_jump;

enum opcodes
{
  opNOP = LAST_ROM_OBJ,
  opDUP,
  opDUP_IF_NIL,
  opDUP_UNLESS_NIL,
  opJUMP_FORWARD_ALWAYS,
  opJUMP_FORWARD_IF_NIL,
  opJUMP_FORWARD_UNLESS_NIL,
  opJUMP_BACKWARD_ALWAYS,
  opJUMP_BACKWARD_IF_NIL,
  opJUMP_BACKWARD_UNLESS_NIL,
  opLOAD_LITERAL,
  opLOAD_VAR,
  opSETQ,
  opCREATE_CLOSURE,
  opCALL,
  opRETURN,
};


START_EXTERN_C

forward_jump  declare_forward_jump  (void);
forward_jump  insert_forward_jump   (forward_jump jmp);
void          resolve_forward_jump  (forward_jump jmp);
backward_jump declare_backward_jump (void);
void          insert_backward_jump  (backward_jump jmp);

void          compile_expression    (obj expr, bool value_context);
void          compile_constant      (obj o);
void          compile_opcode        (uint8_t);

END_EXTERN_C


#endif /* QSL_COMPILER_H */

