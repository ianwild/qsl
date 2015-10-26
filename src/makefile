OBJECTS =
OBJECTS += cons.o
OBJECTS += eval.o
OBJECTS += fexprs.o
OBJECTS += gc.o
OBJECTS += integer.o
OBJECTS += io.o
OBJECTS += main.o
OBJECTS += obj.o
OBJECTS += symbols.o

GENERATED =
GENERATED += qsl
GENERATED += rom-symbols.ci
GENERATED += rom-symbols.h

OPTIMISER_FLAGS = -O6 -flto
WARNING_FLAGS = -W -Wall -Wmissing-prototypes -Wstrict-prototypes -Werror
CFLAGS = $(OPTIMISER_FLAGS) -DUSE_STDIO $(WARNING_FLAGS)
LDFLAGS = $(OPTIMISER_FLAGS)

qsl:	$(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@
	strip $@

rom-symbols.ci:	make-symbols.awk rom.src
	awk -f make-symbols.awk <rom.src >$@

rom-symbols.h:	count-symbols.awk rom.src
	awk -f count-symbols.awk <rom.src >$@

arduino: clean rom-symbols.ci rom-symbols.h
	mkdir arduino
	ln *.[ch]* arduino
	ln arduino.mak arduino/Makefile
	mv arduino/main.c arduino/main.ino
	make -C arduino

clean:
	rm -fr arduino
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
cons.c:	cons.h gc.h obj.h
cons.h:	types.h
eval.c:	cons.h eval.h obj.h
eval.h:	types.h
fexprs.c:	cons.h eval.h fexprs.h obj.h
fexprs.h:	types.h
gc.c:	gc.h obj.h rom-symbols.h
gc.h:	obj.h
integer.c:	integer.h obj.h
integer.h:	types.h
io.c:	io.h obj.h
io.h:	types.h
main.c:	io.h not-arduino.h obj.h symbols.h
obj.c:	cons.h eval.h fexprs.h gc.h integer.h io.h not-arduino.h obj.h rom-symbols.ci rom-symbols.h types.h
obj.h:	types.h
rom-symbols.h:	types.h
symbols.c:	not-arduino.h obj.h rom-symbols.h symbols.h
symbols.h:	types.h
## AUTODEPS END ##
