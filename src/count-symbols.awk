# create "rom-symbols.h", which simply tells the rest of
# the system how many symbols are defined in ROM

BEGIN {
    next_sym = 0;
    arduino_only = -1;
}

/(arduino-only)/ {arduino_only = next_sym;}

NF && ! /^#/ {
    next_sym += 1;
}

END {
    print "#ifndef QSL_ROM_SYMBOLS_H";
    print "#define QSL_ROM_SYMBOLS_H\n";

    printf ("#define ROM_OBJECT_COUNT    OBJECT_C (%d)\n", next_sym - 1);

    print "#endif // QSL_ROM_SYMBOLS_H";
}
