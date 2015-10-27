#! /usr/bin/awk -f

# create "rom-symbols.h", which simply tells the rest of
# the system how many symbols are defined in ROM

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

    printf ("#define LAST_ROM_OBJ    OBJECT_C (%d)\n\n", next_sym - 1);

    print "#endif /* QSL_ROM_SYMBOLS_H */";
}
