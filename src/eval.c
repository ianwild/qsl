#include "announce.h"
#include "compiler.h"
#include "cons.h"
#include "dbg.h"
#include "eval.h"
#include "gc.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"
#include "symbols.h"

START_IMPLEMENTATION

obj current_environment;


#if WITH_TRACE
static char *symname (uint8_t opcode)
{
  static char buf [128];
  uint16_t n;
  const uint8_t *p = get_rom_spelling (opcode, &n);
  char *q = buf;
  while (n--)
    *q++ = *p++;
  *q = '\0';
  return (buf);
}
#endif

static const obj *constants;
static const uint8_t *function_base;
static const uint8_t *current_function;
static uint16_t interpreter_index;
static obj current_closure;
static obj current_lambda;


static void restore_eval_state (void)
{
  if (current_closure)
  {
    if (get_type (current_closure) != closure_type)
      throw_error (bad_type);
    current_lambda = HEADER_FIELD (current_closure, u.closure_val.lambda_obj);
    #if FROZEN_OBJECT_COUNT > 0
    if (current_lambda >= FIRST_FROZEN_OBJECT && current_lambda <= LAST_FROZEN_OBJECT)
    {
      const frozen_hdr *p = get_frozen_header (current_lambda);
      function_base =
        get_frozen_spelling (pgm_read_word_near (&p -> u.lambda_body.opcodes)) + 1;
      constants =
        get_frozen_body (pgm_read_word_near (&p -> u.lambda_body.constants)) + 1;
    }
    else
    #endif
    {
      objhdr *p = get_header (current_lambda);
      function_base =
        wksp_byte_ptr (get_header (p -> u.lambda_body.opcodes)
                       -> u.string_val) + 1;
      constants =
        wksp_obj_ptr (get_header (p -> u.lambda_body.constants)
                      -> u.array_val) + 1;
    }
    current_function = function_base + interpreter_index;
  }
}

static obj get_const (uint8_t idx)
{
  #if FROZEN_OBJECT_COUNT > 0
  if (current_lambda <= LAST_FROZEN_OBJECT)
    return (pgm_read_word_near (constants + idx));
  #endif
  return (constants [idx]);
}

static uint8_t param_0 (void)
{
  #if FROZEN_OBJECT_COUNT > 0
  if (current_lambda <= LAST_FROZEN_OBJECT)
    return (pgm_read_byte_near (current_function));
  #endif
  return (current_function [0]);
}

#if WITH_TRACE
static uint8_t param_1 (void)
{
  #if FROZEN_OBJECT_COUNT > 0
  if (current_lambda <= LAST_FROZEN_OBJECT)
    return (pgm_read_byte_near (current_function + 1));
  #endif
  return (current_function [1]);
}
#endif

static uint8_t next_opcode (void)
{
  #if FROZEN_OBJECT_COUNT > 0
  if (current_lambda <= LAST_FROZEN_OBJECT)
    return (pgm_read_byte_near (current_function++));
  #endif
  return (*current_function++);
}

static void call_lambda (obj fn, uint8_t argc)
{
  obj lambda = obj_NIL;
  switch (get_type (fn))
  {
  case symbol_type:
    lambda = HEADER_FIELD (fn, u.symbol_val.global_fn);
    break;

  case closure_type:
    lambda = fn;
    break;

  default:
    break;
  }

  if (lambda)
  {
    argc &= 0x7f;
    stack_push (create_int (argc));
    TRACE (("calling lambda %04x/%d\n", lambda, argc));
    stack_push (current_environment);
    stack_push (current_closure);
    interpreter_index = (uint16_t) (current_function - function_base);
    stack_push (create_int (interpreter_index));
    current_closure = lambda;
    interpreter_index = 0;
    current_environment = HEADER_FIELD (lambda, u.closure_val.environment);
    restore_eval_state ();
  }
  else
    throw_error (no_fdefn);
}

static obj interpret_bytecodes (void)
{
  for (;;)
  {
    TRACE (("[%03d] %4zd:\t",
            get_stack_depth (), current_function - function_base));
    uint8_t opcode = next_opcode ();
//    printc ('{'); print_int (opcode); printc ('}');
    switch (opcode)
    {
    case opLOAD_NIL:
      TRACE (("LOAD_NIL\n"));
      stack_push (obj_NIL);
      break;

    case opLOAD_T:
      TRACE (("LOAD_T\n"));
      stack_push (obj_T);
      break;

    case opLOAD_LITERAL:
      TRACE (("LOAD_LITERAL %04x\n", get_const (param_0 ())));
      stack_push (get_const (next_opcode ()));
      break;

    case opDROP:
      TRACE (("DROP\n"));
      stack_pop (1);
      break;

    case opSWAP:
      TRACE (("SWAP\n"));
      {
        obj tos = pop_arg ();
        obj nos = pop_arg ();
        stack_push (tos);
        stack_push (nos);
      }
      break;

    case opDUP:
      TRACE (("DUP\n"));
      stack_push (get_arg (0));
      break;

    case opDUP_IF_NIL:
      TRACE (("DUP_IF_NIL\n"));
      if (get_arg (0) == obj_NIL)
        stack_push (get_arg (0));
      break;

    case opDUP_UNLESS_NIL:
      TRACE (("DUP_UNLESS_NIL\n"));
      if (get_arg (0) != obj_NIL)
        stack_push (get_arg (0));
      break;

    case opJUMP_ALWAYS:
      TRACE (("JUMP_ALWAYS %d\n", param_0 ()));
      current_function = function_base + param_0 ();
      break;

    case opJUMP_IF_NIL:
      TRACE (("JUMP_IF_NIL (%04x) %d\n", get_arg (0), param_0 ()));
      if (pop_arg () == obj_NIL)
        current_function = function_base + param_0 ();
      else
        current_function += 1;
      break;

    case opJUMP_UNLESS_NIL:
      TRACE (("JUMP_UNLESS_NIL (%04x) %d\n", get_arg (0), param_0 ()));
      if (pop_arg () != obj_NIL)
        current_function = function_base + param_0 ();
      else
        current_function += 1;
      break;

    case opLOAD_ZERO:
      TRACE (("LOAD_ZERO\n"));
      stack_push (obj_ZERO);
      break;

    case opLOAD_ONE:
      TRACE (("LOAD_ONE\n"));
      stack_push (obj_ZERO + 1);
      break;

    case opLOAD_VAR:
      TRACE (("LOAD_VAR %04x\n", get_const (param_0 ())));
      {
        bool dummy;
        stack_push (symbol_value (get_const (next_opcode ()), &dummy));
      }
      break;

    case opSETQ:
      TRACE (("SETQ %04x %04x\n",
              get_const (param_0 ()), get_arg (0)));
      set_symbol_value (get_const (next_opcode ()), pop_arg ());
      break;

    case opSET_FDEFN:
      TRACE (("SET_FDEFN %04x %04x\n", get_arg (1), get_arg (0)));
      {
        obj fn = pop_arg ();
        obj sym = pop_arg ();
        objhdr *p = get_header (sym);
        p -> u.symbol_val.global_fn = fn;
      }
      break;

    case opCREATE_CONTEXT_BLOCK:
      TRACE (("CREATE_CONTEXT_BLOCK %d\n", param_0 ()));
      {
        uint8_t size = next_opcode ();
        obj new_env = new_extended_object (environment_type, 1 + 2 * size);
        objhdr *p = get_header (new_env);
        wksp_obj_ptr (p -> u.array_val) [1] = current_environment;
        stack_push (obj_ZERO);
        stack_push (new_env);
      }
      break;

    case opBIND_ARGLIST:
      TRACE (("BIND_ARGLIST %d\n", param_0 ()));
      {
        bool listify = false;
        uint8_t size = next_opcode ();
        if (size == 255)
        {
          listify = true;
          size = 1;
        }
        obj new_env = (size
                       ? new_extended_object (environment_type, 1 + 2 * size)
                       : obj_NIL);
        obj old_tos = pop_arg ();
        obj old_nos = pop_arg ();
        obj old_3rd = pop_arg ();
        uint8_t argc = get_int_val (pop_arg ());
        if (listify)
        {
          stack_push (fn_list (&argc));
          argc = 1;
        }
        else
          adjust_argc (&argc, size);
        if (size)
        {
          objhdr *p = get_header (new_env);
          obj *fill_ptr = wksp_obj_ptr (p -> u.array_val) + 1;
          *fill_ptr++ = current_environment;
          current_environment = new_env;
          for (uint8_t i = 1; i <= size; i += 1)
          {
            *fill_ptr++ = get_const (next_opcode ());
            *fill_ptr++ = get_arg (size - i);
          }
        }
        stack_pop (argc);
        stack_push (old_3rd);
        stack_push (old_nos);
        stack_push (old_tos);
      }
      break;

    case opINSERT_BINDING:
      TRACE (("INSERT_BINDING (%04x[%d]: %04x)\n",
              get_arg (1), get_arg (2) - obj_ZERO, get_arg (0)));
      {
        uint8_t idx = 2 + 2 * get_and_incr_arg (2);
        obj sym = get_const (next_opcode ());
        objhdr *p = get_header (get_arg (1));
        wksp_obj_ptr (p -> u.array_val) [idx] = sym;
        wksp_obj_ptr (p -> u.array_val) [idx + 1] = pop_arg ();
      }
      break;

    case opPUSH_CONTEXT:
      TRACE (("PUSH_CONTEXT\n"));
      current_environment = pop_arg ();
      break;

    case opPOP_CONTEXT:
      TRACE (("POP_CONTEXT\n"));
      {
        objhdr *p = get_header (current_environment);
        current_environment = wksp_obj_ptr (p -> u.array_val) [1];
      }
      break;

    case opCREATE_CLOSURE:
      TRACE (("CREATE_CLOSURE %04x (%04x)\n",
              get_arg (0), current_environment));
      if (current_environment)
      {
        objhdr *p;
        obj res = new_object (closure_type, &p);
        objhdr *q = get_header (pop_arg ());
        p -> u.closure_val.environment = current_environment;
        p -> u.closure_val.lambda_obj = q -> u.closure_val.lambda_obj;
        stack_push (res);
      }
      break;

    case opCALL:
      TRACE (("CALL %04x/%d\n",
              get_const (param_0 ()), param_1 ()));
      {
        obj fn = get_const (next_opcode ());
        uint8_t argc = next_opcode ();
        call_lambda (fn, argc);
      }
      break;

    case opRETURN:
      TRACE (("RETURN\n"));
      {
        obj retval = pop_arg ();
        if (get_stack_depth () < 3)
          return (retval);
        interpreter_index = get_int_val (pop_arg ());
        current_closure = pop_arg ();
        current_environment = pop_arg ();
        stack_push (retval);
        restore_eval_state ();
      }
      break;

    default:
      if (opcode <= ROM_OBJECT_COUNT)
      {
        TRACE (("call builtin |%s| with %d args\n",
                symname (opcode), param_0 ()));
        const rom_object *hdr = get_rom_header (opcode);
        built_in_fn fn = (built_in_fn) pgm_read_word_near (&hdr -> global_fn);
        if (! fn || pgm_read_byte_near (&hdr -> is_fexpr))
          throw_error (no_fdefn);
        uint8_t argc = next_opcode ();
        bool void_context = (argc >= 128);
        if (void_context)
          argc -= 128;
        obj res = fn (&argc);
        if (opcode != obj_APPLY)
        {
          stack_pop (argc);
          if (! void_context)
            stack_push (res);
        }
      }
      else
      {
        TRACE (("opcode is %u = 0x%04x\n", opcode, opcode));
        throw_error (compiler_error);
      }
      break;
    }
  }
}

void eval_announce (enum announcement ann)
{
  switch (ann)
  {
  case ann_computation_aborted:
  case ann_shutting_down:
    current_closure = current_environment = obj_NIL;
    break;

  case ann_gc_starting:
    want_obj (current_closure);
    want_obj (current_environment);
    interpreter_index = (uint16_t) (current_function - function_base);
    break;

  case ann_gc_finished:
    restore_eval_state ();
    break;

  default:
    break;
  }
}


obj interpret_top_level (obj closure)
{
  stack_reinit ();
  current_closure = closure;
  interpreter_index = 0;
  current_environment = obj_NIL;
  restore_eval_state ();
  return (interpret_bytecodes ());
}

obj fn_apply (uint8_t *argc)
{
  if (! *argc)
    return (obj_NIL);

  uint8_t n = *argc - 1;
  obj fn = snip_arg (n);

  if (n)
  {
    obj tos = pop_arg ();
    n -= 1;
    while (get_type (tos) == cons_type)
    {
      obj car;
      decons (tos, &car, &tos);
      stack_push (car);
      n += 1;
    }
  }
  *argc = n;

  switch (get_type (fn))
  {
  case rom_symbol_type:
  {
    const rom_object *hdr = get_rom_header (fn);
    built_in_fn fn = (built_in_fn) pgm_read_word_near (&hdr -> global_fn);
    if (! fn || pgm_read_byte_near (&hdr -> is_fexpr))
      throw_error (no_fdefn);

    obj res = fn (argc);
    if (fn == fn_apply)
      res = pop_arg ();
    stack_pop (*argc);
    *argc = 0;
    stack_push (res);
    return (obj_NIL);
  }

  case symbol_type:
  case closure_type:
    call_lambda (fn, n);
    return (obj_NIL);

  default:
    throw_error (no_fdefn);
    return (obj_NIL);
  }
}

END_IMPLEMENTATION
