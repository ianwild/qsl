OBJECTS =
OBJECTS += arrays.o
OBJECTS += compiler.o
OBJECTS += cons.o
OBJECTS += eval.o
OBJECTS += fexprs.o
OBJECTS += gc.o
OBJECTS += integer.o
OBJECTS += io.o
OBJECTS += main.o
OBJECTS += misc.o
OBJECTS += obj.o
OBJECTS += stack.o
OBJECTS += symbols.o

GENERATED =
GENERATED += qsl
GENERATED += rom-symbols.ci
GENERATED += rom-symbols.h

OPTIMISER_FLAGS = -Os -flto
WARNING_FLAGS = -W -Wall -Wmissing-prototypes -Wstrict-prototypes -Werror
CFLAGS = --std=c11 $(OPTIMISER_FLAGS) -DTARGET_ARDUINO=0 $(WARNING_FLAGS)
LDFLAGS = $(OPTIMISER_FLAGS)

.PHONY: clean tar dep tags

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
	rm -f $(OBJECTS) $(GENERATED) *~ *# *.bak *.s
	rm -f ../doc/*~ ../doc/*#

tar:	clean
	tar czf `date +/tmp/qsl.%Y-%m-%d.tgz`   \
		--exclude .git                  \
		--exclude .gitignore            \
		--exclude TAGS                  \
		..

tags:
	find . -name '*.[ch]' -o -name '*.cpp' | etags -

## generic rules


dep:
	makedep *.[ch]

%.h:
	touch -c $@

%.c:
	touch -c $@


## AUTODEPS START ##
arrays.c:	arrays.h cons.h integer.h obj.h stack.h
arrays.h:	target.h types.h
compiler.c:	compiler.h cons.h io.h obj.h stack.h
compiler.h:	rom-symbols.h target.h types.h
cons.c:	cons.h gc.h io.h obj.h stack.h
cons.h:	target.h types.h
embedded.h:	obj.h target.h
eval.c:	compiler.h cons.h dbg.h eval.h fexprs.h io.h obj.h stack.h symbols.h target.h
eval.h:	target.h types.h
fexprs.c:	compiler.h cons.h eval.h fexprs.h io.h obj.h stack.h symbols.h
fexprs.h:	target.h types.h
gc.c:	embedded.h eval.h gc.h io.h obj.h rom-symbols.h
gc.h:	obj.h target.h
hardware.h:	target.h types.h
integer.c:	integer.h io.h obj.h stack.h
integer.h:	target.h types.h
io.c:	cons.h gc.h integer.h io.h obj.h stack.h symbols.h target.h
io.h:	target.h types.h
main.c:	compiler.h embedded.h eval.h gc.h io.h obj.h stack.h symbols.h target.h
misc.c:	integer.h misc.h obj.h stack.h
misc.h:	target.h types.h
obj.c:	arrays.h cons.h dbg.h eval.h fexprs.h gc.h hardware.h integer.h io.h misc.h obj.h rom-symbols.ci rom-symbols.h target.h types.h
obj.h:	target.h types.h
rom-symbols.h:	types.h
stack.c:	obj.h stack.h
stack.h:	target.h types.h
symbols.c:	dbg.h eval.h obj.h rom-symbols.h symbols.h target.h
symbols.h:	target.h types.h
## AUTODEPS END ##
