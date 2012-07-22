# uCaml

 uCaml -- translates OCaml into C language.

## DESCRIPTION

This program translates OCaml into C language. It is based on MinCaml.
The name of this program is uCaml that is called microCaml.

## USAGE

 ucaml [-v] [--gc] [-o FILE] FILE

## OPTIONS

* -o FILE
    write the output to the FILE. By default, ucaml writes the result to standard output.

* --gc
    generates C program that uses Boehm GC as gabage collection. By default, uses reference counter instead.

* -v
    verbose mode, which means, shows log message for debugging.

## HOW TO BUILD

 $ make 

 OCaml 3.12 is required.



