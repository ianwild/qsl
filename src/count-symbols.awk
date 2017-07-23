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
    print "#include \"types.h\"\n";

    if (arduino_only >= 0) {
        print "#if TARGET_ARDUINO"
        printf ("  #define LAST_ROM_OBJ    OBJECT_C (%d)\n", next_sym - 1);
        print "#else"
        printf ("  #define LAST_ROM_OBJ    OBJECT_C (%d)\n", arduino_only - 1);
        print "#endif\n"
    } else {
        printf ("#define LAST_ROM_OBJ    OBJECT_C (%d)\n\n", next_sym - 1);
    }

    print "#endif /* QSL_ROM_SYMBOLS_H */";
}
