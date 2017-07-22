Other than a few immediate objects (15-bit integers and 8-bit
characters), every user-defined object is represented by a "header",
big enough to contain a flag byte and two "object indices".  WIth only
a couple of kilobytes to play with, were not going to get many
distinct objects, so a 16-bit object index is more than wide enough.
That makes each header 5 bytes.  Headers are allocated from the top of
memory, working downwards, and are numbered contiguously with the
built-in objects.

Some objects, notably strings and arrays, need a header and a
variable-length "body".  The bodies are allocated from the bottom of
memory, and work upwards.  A body contains the object number of its
header, and the header of an "extended object" contains a pointer to
the body.  This allows the garbage collector to "slide" bodies down,
to keep the free space contiguous.

Temporary buffers (for I/O, the call stack, working space for the
compiler) are allocated in the same way as normal user objects, but
are released when no longer needed.

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

Note that a lambda expression can't know in advance whether it will be
called in a value or a void context.  It must therefore assume
_value_, and leave it to the called to remove the result if necessary.
As a further consequence, `apply`, though a function and not a fexpr,
must know for itself the call-site context.  It therefore gets quite a
bit of special casing.
