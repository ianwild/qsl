#! /usr/bin/awk -f

BEGIN {
    next_sym = 0;
}

NF && ! /^#/ {
    next_sym += 1;
}

END {
    print "#ifndef QSL_ROM_SYMBOLS_H";
    print "#define QSL_ROM_SYMBOLS_H\n";
    print "#include \"types.h\"\n";

    printf ("#define FIRST_RAM_OBJ    OBJECT_C (%d)\n\n", next_sym);

    print "#endif /* QSL_ROM_SYMBOLS_H */";
}
