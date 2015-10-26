OBJECTS =
OBJECTS += cons.o
OBJECTS += eval.o
OBJECTS += fexprs.o
OBJECTS += gc.o
OBJECTS += integer.o
OBJECTS += io.o
OBJECTS += main.o
OBJECTS += obj.o

GENERATED =
GENERATED += qsl
GENERATED += rom-symbols.ci
GENERATED += rom-symbols.h

CFLAGS = -Os -flto -DUSE_STDIO
LDFLAGS = -Os -flto

qsl:	$(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@
	strip $@

rom-symbols.ci:	make-symbols.awk rom.src
	awk -f make-symbols.awk <rom.src >$@

rom-symbols.h:	count-symbols.awk rom.src
	awk -f count-symbols.awk <rom.src >$@

clean:
	rm -f $(OBJECTS) $(GENERATED) *~ *.bak *.s

tar:	clean
	tar czf `date +/tmp/qsl.%Y-%m-%d.tgz`   \
		--exclude CVS                   \
		.
## generic rules


dep:
	makedep *.[ch]

%.h:
	touch -c $@

%.c:
	touch -c $@


## AUTODEPS START ##
cons.c:	cons.h obj.h
cons.h:	types.h
eval.c:	eval.h obj.h
eval.h:	types.h
fexprs.c:	eval.h fexprs.h obj.h
fexprs.h:	types.h
gc.c:	obj.h rom-symbols.h
integer.c:	integer.h obj.h
integer.h:	types.h
io.c:	io.h obj.h
io.h:	types.h
main.c:	io.h obj.h
obj.c:	cons.h eval.h fexprs.h gc.h integer.h io.h obj.h rom-symbols.ci rom-symbols.h types.h
obj.h:	types.h
rom-symbols.h:	types.h
## AUTODEPS END ##
