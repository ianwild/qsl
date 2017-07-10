#include "compiler.h"
#include "cons.h"
#include "fexprs.h"
#include "eval.h"
#include "io.h"
#include "obj.h"
#include "stack.h"
#include "symbols.h"

static const char PROGMEM this_file [] = __FILE__;

obj eval_progn (obj o, obj res)
{
  while (o != obj_NIL)
  {
    obj car, cdr;
    decons (o, &car, &cdr);
    res = eval_internal (car);
    o = cdr;
  }
  return (res);
}

#if NOT_YET_CONVERTED
static obj split_args (obj args, obj *env)
{
  obj *argv = get_header (args) -> u.array_val;
  *env = argv [2];
  return (argv [1]);
}
#endif

static void compile_progn (obj expr_list, uint8_t for_value)
{
  while (expr_list != obj_NIL)
  {
    obj car;
    decons (expr_list, &car, &expr_list);
    compile_expression (car, (expr_list == obj_NIL) && for_value);
  }
}

obj fe_progn (uint8_t for_value)
{
  compile_progn (get_arg (0), for_value);
  return (obj_NIL);
}

obj fe_cond (uint8_t for_value)
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
      decons (clause, &test, &clause);
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
        if (for_value)
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

obj fe_while (uint8_t for_value)
{
  obj test;
  obj body;
  decons (get_arg (0), &test, &body);
  forward_jump to_test = declare_forward_jump ();
  compile_opcode (opJUMP_ALWAYS);
  to_test = insert_forward_jump (to_test);
  backward_jump loop_start = declare_backward_jump ();
  compile_progn (body, false);
  resolve_forward_jump (to_test);
  compile_expression (test, true);
  compile_opcode (opJUMP_UNLESS_NIL);
  insert_backward_jump (loop_start);
  if (for_value)
    compile_opcode (opLOAD_NIL);
  return (obj_NIL);
}

obj fe_quote (uint8_t for_value)
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

obj fe_setq (uint8_t for_value)
{
  obj sym, car, cdr;
  decons (get_arg (0), &sym, &cdr);
  decons (cdr, &car, &cdr);
  compile_expression (car, true);
  if (for_value)
    compile_opcode (opDUP);
  compile_opcode (opSETQ);
  compile_constant (sym);
  return (obj_NIL);
}

static obj defun_common (obj tag, uint8_t for_value)
{
  obj body = get_arg (0);
  print1 (body);
  obj sym, cdr;
  decons (body, &sym, &cdr);
  if (get_type (sym) != symbol_type)
    throw_error (bad_type);
  objhdr *p;
  obj closure = new_object (closure_type, &p);
  // protect closure across the cons() call
  p -> flags |= gc_fixed;
  {
    p -> u.closure_val.environment = obj_T;
    p -> u.closure_val.lambda_obj = cons (tag, cdr);
  }
  p -> flags &= ~ gc_fixed;
  compile_opcode (opLOAD_LITERAL);
  compile_constant (sym);
  if (for_value)
    compile_opcode (opDUP);
  compile_opcode (opLOAD_LITERAL);
  compile_constant (closure);
  compile_opcode (opSET_FDEFN);

#if NOT_YET_CONVERTED
  obj env;
  obj arglist = split_args (args, &env);
  obj sym, cdr;
  decons (arglist, &sym, &cdr);
  if (get_type (sym) != symbol_type)
    throw_error (bad_type);
  objhdr *p;
  obj closure = new_object (closure_type, &p);

  // protect closure across the cons() call
  p -> flags |= gc_fixed;
  {
    p -> u.closure_val.environment = env;
    p -> u.closure_val.code = cons (tag, cdr);
  }
  p -> flags &= ~ gc_fixed;

  get_header (sym) -> u.symbol_val.global_fn = closure;
  return (sym);
#endif
  return (obj_NIL);
}

obj fe_defun (uint8_t for_value)
{
  return (defun_common (obj_LAMBDA, for_value));
}

obj fe_fexpr (uint8_t for_value)
{
  return (defun_common (obj_FEXPR, for_value));
}

obj fe_and (uint8_t for_value)
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

obj fe_or (uint8_t for_value)
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

static obj let (bool star, uint8_t for_value)
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

  uint16_t i;
  for (i = 0; i < n; i += 1)
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
    //compile_opcode (i);
    compile_constant (one_binding);
  }

  compile_opcode (star ? opDROP : opPUSH_CONTEXT);
  compile_opcode (opDROP);

  compile_progn (body, for_value);
  compile_opcode (opPOP_CONTEXT);
  return (obj_NIL);
}

obj fe_let (uint8_t for_value)
{
  return (let (false, for_value));
}

obj fe_let_star (uint8_t for_value)
{
  return (let (true, for_value));
}

obj fe_apply (uint8_t for_value)
{
#if NOT_YET_CONVERTED
  obj env;
  obj fn = split_args (args, &env);
  decons (fn, &fn, &args);
  return (apply_internal (eval_internal (fn), args));
#endif
  (void) for_value;
  return (obj_NIL);
}
