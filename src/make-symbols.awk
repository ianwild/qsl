# create "rom-symbols.ci", which contains a pair of arrays:
#   bytes[] is just the (len,spelling) pairs for the symbols in ROM,
#   rom_symbols[] is a list of rom_object structures.

BEGIN {
    next_sym = 0;
}

NF && ! /^#/ {
    lisp_name = $1;
    c_fn = $2;
    if (c_fn == "fn" || c_fn == "fe")
    {
        c_fn = c_fn "_" lisp_name;
        gsub (/-/, "_", c_fn);
    }
    else if (c_fn == "" || c_fn == "-")
        c_fn = "NULL";
    symbol_table [next_sym] = lisp_name;
    c_name [lisp_name] = c_fn;
    next_sym += 1;
}

END {
    print "static const PROGMEM uint8_t bytes [] = {";
    idx = 0;
    for (i = 0; i < next_sym; i += 1) {
        lisp_name = symbol_table [i];
        len = length(lisp_name);
        name_offset [lisp_name] = idx;
        printf ("  /* %3d */  %d,", idx, len);
        for (j = 1; j <= len; j += 1) {
            ch = substr (lisp_name, j, 1);
            if (ch == "\\" || ch == "'")
                ch = "\\" ch;
            printf (" '%s',", ch);
        }
        idx += len + 1;
        printf ("\n");
    }
    print "};\n";

    print "static const PROGMEM rom_object rom_symbols [] = {";
    for (i = 0; i < next_sym; i += 1) {
        lisp_name = symbol_table [i];
        spelling = name_offset [lisp_name];
        fn = c_name [lisp_name];
        printf ("  /* %3d = %-8s */  ", i, lisp_name);
        printf ("{bytes + %3d, %s, %d},\n", spelling, fn, fn ~ /^fe_/);
    }
    print "};\n";
}


