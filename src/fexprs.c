#include "cons.h"
#include "fexprs.h"
#include "eval.h"
#include "io.h"
#include "obj.h"
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

static obj split_args (obj args, obj *env)
{
  obj *argv = get_header (args) -> u.array_val;
  *env = argv [2];
  return (argv [1]);
}

obj fe_progn (uint8_t for_value)
{
  obj expr_list = get_arg (0);
  while (expr_list != obj_NIL)
  {
    obj car;
    decons (expr_list, &car, &expr_list);
    compile_expression (car, (expr_list == obj_NIL) && for_value);
  }
}

obj fe_cond (obj args)
{
  obj env;
  obj elist = split_args (args, &env);
  while (elist != obj_NIL)
  {
    obj clause, car, cdr;
    decons (elist, &clause, &elist);
    decons (clause, &car, &cdr);
    obj res = eval_internal (car);
    if (res != obj_NIL)
      return (eval_progn (cdr, res));
  }
  return (obj_NIL);
}


obj fe_while (uint8_t for_value)
{
  obj test;
  obj body;
  decons (pop_arg (), &test, &body);
  stack_push (body);
  forward_jump to_test = declare_forward_jump ();
  backward_jump loop_start = declare_backward_jump ();
  compile_opcode (opJUMP_FORWARD_ALWAYS);
  to_test = insert_forward_jump (to_test);
  fe_progn (false);
  resolve_forward_jump (to_test);
  compile_expression (test, true);
  compile_opcode (opJUMP_BACKWARD_UNLESS_NIL);
  insert_backward_jump (loop_start);
  if (for_value)
  {
    compile_opcode (opLOAD_LITERAL);
    compile_constant (obj_NIL);
  }
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
  compile_expression (cdr, true);
  if (for_value)
    compile_opcode (opDUP);
  compile_opcode (opSETQ);
  compile_constant (sym);
  return (obj_NIL);
}

static obj defun_common (obj args, obj tag)
{
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
}

obj fe_defun (obj args)
{
  return (defun_common (args, obj_LAMBDA));
}

obj fe_fexpr (obj args)
{
  return (defun_common (args, obj_FEXPR));
}

obj fe_and (uint8_t for_value)
{
  obj expr_list = get_arg (0);
  if (expr_list == obj_NIL)
  {
    if (for_value)
    {
      compile_opcode (opLOAD_LITERAL);
      compile_constant (obj_T);
    }
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
	compile_opcode (opJUMP_FORWARD_IF_NIL);
	insert_forward_jump (to_finish);
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
    {
      compile_opcode (opLOAD_LITERAL);
      compile_constant (obj_NIL);
    }
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
	compile_opcode (opJUMP_FORWARD_UNLESS_NIL);
	insert_forward_jump (to_finish);
      }
    }
  }
  return (obj_NIL);
}

static obj let (obj args, bool star)
{
  obj bindings;
  obj elist = split_args (args, &bindings);
  decons (elist, &bindings, &elist);
  uint16_t argc = internal_len (bindings);
  if (argc == 0)
    return (eval_progn (elist, obj_NIL));
  obj new_env = new_extended_object (environment_type, 1 + 2 * argc);
  objhdr *p = get_header (new_env);
  p -> u.array_val [1] = current_environment;
  if (star)
    current_environment = new_env;
  else
    p -> flags |= gc_fixed;
  uint16_t i = 2;
  while (bindings)
  {
    obj one_binding;
    decons (bindings, &one_binding, &bindings);
    if (get_type (one_binding) == cons_type)
    {
      obj var, val;
      decons (one_binding, &var, &one_binding);
      decons (one_binding, &val, &one_binding);
      val = eval_internal (val);
      p -> u.array_val [i] = var;
      p -> u.array_val [i + 1] = val;
    }
    else
      p -> u.array_val [i] = one_binding;
    i += 2;
  }
  if (! star)
  {
    current_environment = new_env;
    p -> flags &= ~gc_fixed;
  }
  obj res = eval_progn (elist, obj_NIL);
  current_environment = p -> u.array_val [1];

  return (res);
}

obj fe_let (obj args)
{
  return (let (args, false));
}

obj fe_let_star (obj args)
{
  return (let (args, true));
}

obj fe_apply (obj args)
{
  obj env;
  obj fn = split_args (args, &env);
  decons (fn, &fn, &args);
  return (apply_internal (eval_internal (fn), args));
}
