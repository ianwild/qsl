BEGIN {
    print "#define TARGET_ARDUINO\t1"
    print "#define REPL_IS_MAIN\t1"
}

/^QSL_OPTIONS.*-DWITH_/ {
    sub (/QSL_OPTIONS.*-D/, "#define ");
    sub (/=/, "\t");
    print;
}
