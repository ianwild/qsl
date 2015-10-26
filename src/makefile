OBJECTS =
OBJECTS += cons.o
OBJECTS += eval.o
OBJECTS += fexprs.o
OBJECTS += gc.o
OBJECTS += integer.o
OBJECTS += io.o
OBJECTS += main.o
OBJECTS += obj.o

CFLAGS = -Os -flto
LDFLAGS = -Os -flto

qsl:	$(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@
	strip $@

clean:
	rm -f $(OBJECTS) qsl *~ *.bak

tar:	clean
	tar czf `date +/tmp/qsl.%Y-%m-%d.tgz`   \
		--exclude CVS                   \
		.
dep:
	makedep *.c *.h

## AUTODEPS START ##
cons.c:	cons.h obj.h
eval.c:	eval.h obj.h
fexprs.c:	eval.h fexprs.h obj.h
gc.c:	obj.h
integer.c:	integer.h obj.h
io.c:	io.h obj.h
main.c:	io.h
obj.c:	gc.h obj.h
cons.h:	types.h
eval.h:	types.h
fexprs.h:	types.h
integer.h:	types.h
io.h:	types.h
obj.h:	types.h
## AUTODEPS END ##
