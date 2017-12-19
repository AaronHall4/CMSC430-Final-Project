CMSC430 Class Project: Scheme Compiler

This is a Scheme compiler, developed over the course of the semester for CMSC430 (compilers). I used
the reference solution version of closure-convert.rkt, because mine didn't work properly and didn't
use the 0-CFA analysis (which I thought was cool to have). The other main source files (cps.rkt, 
desugar.rkt, and top-level.rkt) are the version that I turned in for the relevant assignments, with
some changes as necessary. I also used the utility files provided to us (utils.rkt, tests.rkt, and 
header.cpp), with varying degrees of modification to accomodate for changes made during the final
project.

This compiler uses the Boehm-Demers-Weiser garbage collector to manage memory. It also uses the HAMT
data structure to implement hashes and sets. The compilation process uses 9 main passes:
    1) Converting the top-level code into a "desugarable" form.
    2) Desugaring.
    3) IR Simplification
    4) Assignment conversion.
    5) ANF (administrative normal form) conversion.
    6) CPS (continuation-passing style) conversion.
    7) Closure conversion.
    8) LLVM IR emission.
    9) Compiling LLVM IR to machine code.



