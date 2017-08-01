BEGIN {
    print "#define TARGET_ARDUINO 1"
    print "#define REPL_IS_MAIN   1"
}

/^QSL_OPTIONS.*-DWITH.*=1/ {
    sub (/QSL_OPTIONS.*-D/, "");
    sub (/=1/, "");
    print "#define " $0 " 1"
}
