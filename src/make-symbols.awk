#! /usr/bin/awk -f

BEGIN {
    next_sym = 0;
    for (i = 1; i < 256; i += 1)
	ascii [sprintf ("%c", i)] = i;
}

NF && ! /^#/ {
    lisp_name = $1;
    c_fn = $2;
    if (c_fn == "fn" || c_fn == "fe")
	c_fn = c_fn "_" lisp_name;
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
	printf ("  /* %3d */  ", idx + len);
	for (j = 1; j <= len; j += 1)
	    printf ("%d, ", ascii [substr (lisp_name, j, 1)]);
	idx += len;
	name_offset [lisp_name] = idx;
	printf ("%d,\n", len);
	idx += 1;
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


