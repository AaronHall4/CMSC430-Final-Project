******************************************
* CMSC430 Class Project: Scheme Compiler *
******************************************

------------
| Overview |
------------

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

A 0-CFA optimization step is used for optimizing in the closure conversion stage, but I typically set it
to have a short timeout (e.g., 1 second) to make compiling take less time. The constants that enable/disable
this 0-CFA and change its timeout are found at the top of closure-convert.rkt.

------------------------------------
| Running and testing the compiler |
------------------------------------

The easiest way to compile a .scm file is to run the command 

                            ./compile_scheme.rkt <filename.scm> 

from the command line, which will output an executable called <filename> (i.e., the name of the Scheme file,
with the .scm extension missing). Various compilation functions that can be used from DrRacket can be found in
compile.rkt. The testing infrastructure appears in tests.rkt, and behaves exactly the same as that of 
previous assignments. Constants such as paths to the Boehm GC library and clang are found at the top of 
utils.rkt, and can/should be changed as appropriate.

----------------------------------
| Supported primitive operations |
----------------------------------

The following is a complete list of primitive operationss supported by this compiler. In general, the program
will terminate with an error message if an argument with an improper type is provided to the operation.

     = : Int x Int -> Bool
       Tests whether two numbers are equal.
     > : Int x Int -> Bool
       Tests whether the first argument is strictly greater than the second.
     < : Int x Int -> Bool
       Tests whether the first argument is strictly less than the second.
    >= : Int x Int -> Bool
       Tests whether the first argument is greater than or equal to the second.
    <= : Int x Int -> Bool
       Tests whether the first argument is greater than or equal to the second.
     + : Int ... -> Int
       Adds all arguments together. If zero arguments are passed, returns 0. If one argument is passed, returns
       that argument.
     - : Int x Int (x Int ...) -> Int
       Subtracts every argument (starting from the second) from the first, in order. For example, (- 1 2 3) 
       returns -4.
     * : Int ... -> Int
       Multiplies all arguments together. If zero arguments are passed, returns 1. If one argument is passed, 
       returns that argument.
     / : Int ... -> Int
       Performs integer division (with truncation) on the arguments, in order. For example, (/ 40 10 2) returns
       2. If zero arguments are passed, returns 1. If one argument is passed, returns that argument. If 0 is
       passed as an argument after the first, prints "fatal error: division by zero" and terminates the program.
  cons : Any x Any -> Cons
       Creates a cons cell containing the two arguments.
   car : Cons -> Any
       Returns the first element of a cons cell.
   cdr : Cons -> Any
       Returns the second element of a cons cell.
  list : Any ... -> List
       Creates a list (i.e., a series of nested cons cells terminated by '()) out of the provided arguments. If
       zero arguments are passed, returns the empty list, represented by '().
 first : List -> Any
       Returns the first element of a list. WARNING: program will crash if the list has fewer than 1 element.
second : List -> Any
       Returns the second element of a list. WARNING: program will crash if the list has fewer than 2 elements.
 third : List -> Any
       Returns the third element of a list. WARNING: program will crash if the list has fewer than 3 elements.
fourth : List -> Any
       Returns the fourth element of a list. WARNING: program will crash if the list has fewer than 4 elements.
 fifth : List -> Any
       Returns the fifth element of a list. WARNING: program will crash if the list has fewer than 5 elements.
  last : List -> Any
       Returns the last element of a list, or '() if the list is empty. For example, (last (list 1 2 3)) returns
       3 and (last (list)) returns '().
length : List -> Int
        Returns the length of a list. For example, (length (list 1 2 3)) returns 3 and (length (list)) returns 0.
