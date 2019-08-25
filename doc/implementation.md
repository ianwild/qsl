# Random notes on the implementation

Other than a few immediate objects (15-bit integers and 8-bit
characters), every user-created object is represented by a "header",
big enough to contain a flag byte and two "object indices".  With only
a couple of kilobytes to play with, we're not going to get many
distinct objects, so a 16-bit object index is more than wide enough.
That makes each header 5 bytes.  Headers are allocated from the top of
memory, working downwards.  (The first few object indices are
reserved for built-in objects, resident in flash.  RAM-resident object
headers are numbered consecutively with the built-in objects.)

Some objects, notably strings and arrays, need a header and a
variable-length "body".  The bodies are allocated from "string space"
at the bottom of memory, and work upwards.  At some point, the header
table growing downward will meet the string space growing upwards, at
which point the garbage collector is triggered.

A body contains the object number of its header, and the header of an
"extended object" contains a pointer to the body.  This allows the
garbage collector to "slide" bodies down, to keep the free space
contiguous.

Temporary buffers (for I/O, the environment stack for lexical
bindings, working space for the compiler) are allocated in the same
way as normal user objects, but are released when no longer needed.

Built-in functions (`fn_*`) take a single parameter, `uint8_t *argc`,
where `argc` indicates how many arguments `interpret_bytecodes()` has
pushed.  This can be used as-is, or modified (up or down) with
`adjust_argc()` to either lose excessive arguments or to pad with
`nil`s.  When the function returns, `interpret_bytecodes()` will
remove the remaining arguments, and (if the call was in a value
context) push the returned value.

Since they live in the same table, built-in fexprs (`fe_*`, being
functions with unusual compile-time behaviour) must have the same
signature.  However, instead of a count of how many arguments are
passed, the parameter pointer is either NULL or non-NULL, depending on
whether the fexpr should compile for a void or a value context.  On
the stack is a _single_ `obj`, the `cdr` of the fexpr form, which the
fexpr must use to perform its magic.

Note that a lambda expression can't know in advance whether it will be
called in a value or a void context.  It must therefore assume
_value_, and leave it to the caller to remove the result if necessary.
This means that most of the time `opCALL` will be followed by an
`opDROP`.  As a further consequence, `apply`, though a function and
not a fexpr, must know for itself the call-site context, so it gets
quite a bit of special casing.

If "frozen objects" are used, they are initialised as a snapshot of RAM,
and so have a similar format.  They live in flashm squeezed between the
ROM-resident built-ins and the RAM-resident objects.  Their body pointers,
if any, _also_ point into ROM, so they are necessarily immutable.
(If a "frozen" symbol is unbound, however, it _can_ be assigned in RAM.)

