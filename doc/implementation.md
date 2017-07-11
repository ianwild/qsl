Built-in functions (`fn_`) take a single parameter, `uint8_t *argc`,
which indicates how many arguments `interpret_bytecodes()` has pushed.
This can be used as-is, or modified (up or down) with `adjust_argc()`
to either lose excessive arguments or to pad wil `NIL`s.  When the
function returns, `interpret_bytecodes()` will remove the remaining
arguments, and (if the call was in a value context) push the returned
value.

Since they live in the same table, built-in fexprs (`fe_`) must have
the same signature.  However, in this case there is a _single_ `obj`
pushed onto the stack, the `cdr` of the fexpr form.  Instead of a
count of how many arguments are passed, instead the parameter pointer
is either NULL or non-NULL, depending on whether the fexpr should
compile for a void or a value context.

