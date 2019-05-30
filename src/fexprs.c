#include "compiler.h"
#include "cons.h"
#include "fexprs.h"
#include "eval.h"
#include "integer.h"
#include "obj.h"
#include "stack.h"

START_IMPLEMENTATION

static void compile_progn (obj expr_list, uint8_t *for_value)
{
  if (expr_list == obj_NIL)
    compile_expression (expr_list, !! for_value);
  else
    while (expr_list != obj_NIL)
    {
      obj car;
      decons (expr_list, &car, &expr_list);
      compile_expression (car, (expr_list == obj_NIL) && for_value);
    }
}

obj fe_progn (uint8_t *for_value)
{
  compile_progn (get_arg (0), for_value);
  return (obj_NIL);
}

obj fe_cond (uint8_t *for_value)
{
  obj expr_list = get_arg (0);
  if (expr_list == obj_NIL)
  {
    if (for_value)
      compile_opcode (opLOAD_NIL);
  }
  else
  {
    forward_jump to_finish = declare_forward_jump ();

    while (expr_list != obj_NIL)
    {
      obj clause;
      decons (expr_list, &clause, &expr_list);
      obj test;
      if (get_type (clause) == cons_type)
        decons (clause, &test, &clause);
      else
      {
        test = clause;
        clause = obj_NIL;
      }
      if (expr_list == obj_NIL)
      {
        // last clause
        if (clause == obj_NIL)
          compile_expression (test, for_value);
        else
        {
          compile_expression (test, true);
          if (for_value)
            compile_opcode (opDUP_IF_NIL);
          compile_opcode (opJUMP_IF_NIL);
          to_finish = insert_forward_jump (to_finish);
          compile_progn (clause, for_value);
        }
      }
      else
      {
        // not last clause
        compile_expression (test, true);
        if (for_value && clause == obj_NIL)
          compile_opcode (opDUP_UNLESS_NIL);
        if (clause == obj_NIL)
        {
          compile_opcode (opJUMP_UNLESS_NIL);
          to_finish = insert_forward_jump (to_finish);
        }
        else
        {
          forward_jump to_next = declare_forward_jump ();
          compile_opcode (opJUMP_IF_NIL);
          to_next = insert_forward_jump (to_next);
          compile_progn (clause, for_value);
          compile_opcode (opJUMP_ALWAYS);
          to_finish = insert_forward_jump (to_finish);
          resolve_forward_jump (to_next);
        }
      }
    }
    resolve_forward_jump (to_finish);
  }
  return (obj_NIL);
}

obj fe_while (uint8_t *for_value)
{
  obj test;
  obj body;
  decons (get_arg (0), &test, &body);
  forward_jump to_test = declare_forward_jump ();
  compile_opcode (opJUMP_ALWAYS);
  to_test = insert_forward_jump (to_test);
  backward_jump loop_start = declare_backward_jump ();
  compile_progn (body, NULL);
  resolve_forward_jump (to_test);
  compile_expression (test, true);
  compile_opcode (opJUMP_UNLESS_NIL);
  insert_backward_jump (loop_start);
  if (for_value)
    compile_opcode (opLOAD_NIL);
  return (obj_NIL);
}

obj fe_quote (uint8_t *for_value)
{
  if (for_value)
  {
    obj car, cdr;
    decons (get_arg (0), &car, &cdr);
    compile_opcode (opLOAD_LITERAL);
    compile_constant (car);
  }
  return (obj_NIL);
}

static void variable_needed (obj s)
{
  if (s <= obj_T ||
      (s > LAST_ROM_OBJ && get_type (s) != symbol_type))
    throw_error (var_needed);
}

obj fe_setq (uint8_t *for_value)
{
  obj sym, car, cdr;
  decons (get_arg (0), &sym, &cdr);
  variable_needed (sym);
  decons (cdr, &car, &cdr);
  compile_expression (car, true);
  if (for_value)
    compile_opcode (opDUP);
  compile_opcode (opSETQ);
  compile_constant (sym);
  return (obj_NIL);
}

void compile_lambda_body (obj body)
{
  obj args;
  decons (body, &args, &body);
  if (args == obj_NIL || get_type (args) == cons_type)
  {
    uint16_t argc = internal_len (args);
    compile_opcode (opBIND_ARGLIST);
    compile_opcode (argc);
    while (args)
    {
      obj arg;
      decons (args, &arg, &args);
      variable_needed (arg);
      compile_constant (arg);
    }
  }
  else
  {
    compile_opcode (opBIND_ARGLIST);
    compile_opcode (255);
    variable_needed (args);
    compile_constant (args);
  }
  compile_progn (body, (uint8_t *) "y");
}

obj fe_defun (uint8_t *for_value)
{
  obj body = get_arg (0);
  obj sym, cdr;
  decons (body, &sym, &cdr);
  if (get_type (sym) != symbol_type)
    throw_error (bad_type);
  objhdr *p;
  obj closure = new_object (closure_type, &p);
  // protect closure across the cons() call
  FIX_OBJ (p);
  {
    p -> u.closure_val.environment = obj_T;
    p -> u.closure_val.lambda_obj = cons (obj_LAMBDA, cdr);
  }
  RELEASE_OBJ (p);
  compile_opcode (opLOAD_LITERAL);
  compile_constant (sym);
  if (for_value)
    compile_opcode (opDUP);
  compile_opcode (opLOAD_LITERAL);
  compile_constant (closure);
  compile_opcode (opCREATE_CLOSURE);
  compile_opcode (opSET_FDEFN);

  return (obj_NIL);
}

obj fe_lambda (uint8_t *for_value)
{
  if (for_value)
  {
    obj body = get_arg (0);
    objhdr *p;
    obj closure = new_object (closure_type, &p);
    // protect closure across the cons() call
    FIX_OBJ (p);
    {
      p -> u.closure_val.environment = obj_T;
      p -> u.closure_val.lambda_obj = cons (obj_LAMBDA, body);
    }
    RELEASE_OBJ (p);
    compile_opcode (opLOAD_LITERAL);
    compile_constant (closure);
    compile_opcode (opCREATE_CLOSURE);
  }
  return (obj_NIL);
}

#if 0
obj fe_fexpr (uint8_t *for_value)
{
  return (defun_common (obj_FEXPR, for_value));
}
#endif

obj fe_and (uint8_t *for_value)
{
  obj expr_list = get_arg (0);
  if (expr_list == obj_NIL)
  {
    if (for_value)
      compile_opcode (opLOAD_T);
  }
  else
  {
    forward_jump to_finish = declare_forward_jump ();

    while (expr_list != obj_NIL)
    {
      obj car;
      decons (expr_list, &car, &expr_list);
      if (expr_list == obj_NIL)
      {
        compile_expression (car, for_value);
        resolve_forward_jump (to_finish);
      }
      else
      {
        compile_expression (car, true);
        if (for_value)
          compile_opcode (opDUP_IF_NIL);
        compile_opcode (opJUMP_IF_NIL);
        to_finish = insert_forward_jump (to_finish);
      }
    }
  }
  return (obj_NIL);
}

obj fe_or (uint8_t *for_value)
{
  obj expr_list = get_arg (0);
  if (expr_list == obj_NIL)
  {
    if (for_value)
      compile_opcode (opLOAD_NIL);
  }
  else
  {
    forward_jump to_finish = declare_forward_jump ();

    while (expr_list != obj_NIL)
    {
      obj car;
      decons (expr_list, &car, &expr_list);
      if (expr_list == obj_NIL)
      {
        compile_expression (car, for_value);
        resolve_forward_jump (to_finish);
      }
      else
      {
        compile_expression (car, true);
        if (for_value)
          compile_opcode (opDUP_UNLESS_NIL);
        compile_opcode (opJUMP_UNLESS_NIL);
        to_finish = insert_forward_jump (to_finish);
      }
    }
  }
  return (obj_NIL);
}

static obj let (bool star, uint8_t *for_value)
{
  obj body = get_arg (0);
  obj bindings;
  decons (body, &bindings, &body);
  uint16_t n = internal_len (bindings);
  if (n == 0)
  {
    compile_progn (body, for_value);
    return (obj_NIL);
  }

  compile_opcode (opCREATE_CONTEXT_BLOCK);
  compile_opcode (n);

  if (star)
  {
    compile_opcode (opDUP);
    compile_opcode (opPUSH_CONTEXT);
  }

  for (uint16_t i = 0; i < n; i += 1)
  {
    obj one_binding;
    decons (bindings, &one_binding, &bindings);
    if (get_type (one_binding) == cons_type)
    {
      obj sym, val;
      decons (one_binding, &sym, &one_binding);
      decons (one_binding, &val, &one_binding);
      compile_expression (val, true);
      one_binding = sym;
    }
    else
      compile_opcode (opLOAD_NIL);
    compile_opcode (opINSERT_BINDING);
    variable_needed (one_binding);
    compile_constant (one_binding);
  }

  compile_opcode (star ? opDROP : opPUSH_CONTEXT);
  compile_opcode (opDROP);

  compile_progn (body, for_value);
  compile_opcode (opPOP_CONTEXT);
  return (obj_NIL);
}

obj fe_let (uint8_t *for_value)
{
  return (let (false, for_value));
}

obj fe_let_star (uint8_t *for_value)
{
  return (let (true, for_value));
}

END_IMPLEMENTATION
