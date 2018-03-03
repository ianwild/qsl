OBJECTS :=
OBJECTS += announce.o
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

GENERATED :=
GENERATED += qsl
GENERATED += rom-symbols.ci
GENERATED += rom-symbols.h
GENERATED += qsl-options.h

QSL_OPTIONS :=
QSL_OPTIONS += -DTARGET_ARDUINO=0
QSL_OPTIONS += -DPRINT_WITH_QUOTES=1
QSL_OPTIONS += -DWITH_COMPILER_STATS=1
QSL_OPTIONS += -DWITH_MEMSTATS=1
QSL_OPTIONS += -DWITH_RC_SCRIPT=1
QSL_OPTIONS += -DWITH_TRACE=1
QSL_OPTIONS += -DWITH_THROW_LOCATION=1

OPTIMISER_FLAGS := -Os -flto
WARNING_FLAGS := -W -Wall -Wmissing-prototypes -Wstrict-prototypes -Werror
CFLAGS := --std=c11 $(OPTIMISER_FLAGS) $(WARNING_FLAGS) $(QSL_OPTIONS)
LDFLAGS := $(OPTIMISER_FLAGS)

.PHONY: clean tar dep tags upload

qsl:	$(OBJECTS)
	$(CC) $(LDFLAGS) $^ -o $@

rom-symbols.ci:	make-symbols.awk rom.src
	awk -f make-symbols.awk <rom.src >$@

rom-symbols.h:	count-symbols.awk rom.src
	awk -f count-symbols.awk <rom.src >$@

qsl-options.h:
	echo '// this file will be over-written' >$@

lib:	clean rom-symbols.ci rom-symbols.h
	mkdir -p QSL/src
	cp *.[ch]* QSL/src
	for i in QSL/src/*.c; do mv $$i $${i%.c}.cpp; done
	awk -f extract-options.awk <arduino.mak >QSL/src/qsl-options.h
	cp lib-etc/QSL.h QSL/src
	cp lib-etc/library.properties QSL

arduino: clean rom-symbols.ci rom-symbols.h qsl-options.h
	mkdir arduino
	ln *.[ch]* arduino
	ln arduino.mak arduino/Makefile
	mv arduino/main.c arduino/main.ino
	for i in arduino/*.c; do mv $$i $${i%.c}.cpp; done
	make -C arduino

upload:
	make -C arduino upload

clean:
	rm -fr arduino QSL
	rm -f $(OBJECTS) $(GENERATED) *~ *# *.bak *.s core
	rm -f lib-etc/*~ lib-etc/*#
	rm -f ../doc/*~ ../doc/*#

tar:	clean
	tar czf `date +/tmp/qsl.%Y-%m-%d.tgz`   \
		--exclude .git                  \
		--exclude .gitignore            \
		--exclude TAGS                  \
		--directory ..                  \
		.

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
announce.c:	announce.h dbg.h
announce.h:	target.h
arrays.c:	arrays.h cons.h integer.h obj.h stack.h
arrays.h:	target.h types.h
compiler.c:	announce.h buffer-limits.h compiler.h cons.h dbg.h fexprs.h gc.h io.h obj.h stack.h
compiler.h:	obj.h rom-symbols.h target.h types.h
cons.c:	cons.h dbg.h obj.h stack.h
cons.h:	target.h types.h
eval.c:	announce.h compiler.h cons.h dbg.h eval.h gc.h integer.h obj.h stack.h symbols.h
eval.h:	target.h types.h
fexprs.c:	compiler.h cons.h eval.h fexprs.h integer.h obj.h stack.h
fexprs.h:	target.h types.h
gc.c:	announce.h dbg.h gc.h obj.h rom-symbols.h stack.h
gc.h:	target.h types.h
hardware.h:	target.h types.h
integer.c:	dbg.h integer.h io.h obj.h stack.h
integer.h:	target.h types.h
io.c:	announce.h buffer-limits.h cons.h dbg.h gc.h integer.h io.h obj.h serial_io.h stack.h symbols.h
io.h:	target.h types.h
main.c:	announce.h compiler.h dbg.h eval.h gc.h io.h obj.h stack.h
misc.c:	integer.h misc.h obj.h stack.h
misc.h:	target.h types.h
obj.c:	announce.h arrays.h buffer-limits.h compiler.h cons.h dbg.h eval.h fexprs.h gc.h hardware.h integer.h io.h misc.h obj.h rom-symbols.ci rom-symbols.h stack.h
obj.h:	target.h types.h
rom-symbols.h:	types.h
serial_io.h:	target.h types.h
stack.c:	announce.h buffer-limits.h dbg.h gc.h obj.h stack.h
stack.h:	target.h types.h
symbols.c:	dbg.h eval.h obj.h rom-symbols.h symbols.h
symbols.h:	target.h types.h
target.h:	qsl-options.h
types.h:	target.h
## AUTODEPS END ##
