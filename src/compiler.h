#ifndef QSL_COMPILER_H
#define QSL_COMPILER_H

#include "rom-symbols.h"
#include "target.h"
#include "types.h"

START_EXTERN_C

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

#endif // TARGET_ARDUINO

enum __attribute__ ((packed)) opcodes
{
  opDROP = LAST_ROM_OBJ + 1,
  opSWAP,
  opDUP,
  opDUP_IF_NIL,
  opDUP_UNLESS_NIL,
  opJUMP_ALWAYS,
  opJUMP_IF_NIL,
  opJUMP_UNLESS_NIL,
  opLOAD_LITERAL,
  opLOAD_NIL,
  opLOAD_T,
  opLOAD_ZERO,
  opLOAD_ONE,
  opLOAD_VAR,
  opSETQ,
  opSET_FDEFN,
  opCREATE_CONTEXT_BLOCK,
  opINSERT_BINDING,
  opBIND_ARGLIST,
  opPUSH_CONTEXT,
  opPOP_CONTEXT,
  opCREATE_CLOSURE,
  opCALL,
  opRETURN
};


forward_jump   declare_forward_jump   (void);
forward_jump   insert_forward_jump    (forward_jump jmp);
void           resolve_forward_jump   (forward_jump jmp);
backward_jump  declare_backward_jump  (void);
void           insert_backward_jump   (backward_jump jmp);

obj            compile_top_level      (obj expr);
void           compile_expression     (obj expr, bool value_context);
void           compile_constant       (obj o);
void           compile_opcode         (uint8_t);

void           compiler_init          (void);
void           compiler_report        (void);

uint8_t        get_longest_opcodes    (void);
uint8_t        get_longest_constants  (void);

END_EXTERN_C


#endif // QSL_COMPILER_H
