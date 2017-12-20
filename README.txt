******************************************
* CMSC430 Class Project: Scheme Compiler *
******************************************

I pledge on my honor that I have not given or received any unauthorized assistance on this examination
or assignment. --Aaron M Hall

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

                        racket compile_scheme.rkt <filename.scm> 

from the command line, which will output an executable called <filename> (i.e., the name of the Scheme file,
with the .scm extension missing). Various compilation functions that can be used from DrRacket can be found in
compile.rkt. The testing infrastructure appears in tests.rkt, and behaves exactly the same as that of 
previous assignments. Constants such as paths to the Boehm GC library and clang are found at the top of 
utils.rkt, and can/should be changed as appropriate.

Here is an example run of the compilation program, and its output. It should work from the directory containing
this README:
        > racket compile_scheme.rkt tests/public/amb.scm
        > tests/public/amb
        < '(solution . (3 . (4 . (5 . ()))))

----------------------------------
| Supported primitive operations |
----------------------------------

The following is a complete list of primitive operationss supported by this compiler. In general, the program
will terminate with an error message if an argument with an improper type is provided to the operation. A list of
primitives that I would like to implement one day but didn't yet for some reason can be found in prims_to_implement.txt.

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
         drop : List x Int -> List
              (drop lst n) returns the tail of lst, starting at the element of index n (0 <= n <= (length lst)). 
              WARNING: program will crash if the provided n is out of range.
    list-tail : List x Int -> List
              Identical to drop.
   drop-right : List x Int -> List
              (drop-right lst n) keeps the first (n - 1) elements of lst and removes the rest. WARNING: program will
              crash if the provided n is out of range.
         take : List x Int -> List
              Identical to drop-right.
       member : Any x List -> List
              (member v lst) returns the tail of lst starting with the first occurrence of v. Equality is checked using
              equal?. If v is not found in lst, including the case where lst is '(), #f is returned. For example, 
              (member 3 (list 1 2 3 4 5 6)) returns '(3 4 5 6).
         memv : Any x List -> List
              Behaves the same as member, except eqv? is used to check for equality instead of equal?.
          map : Proc x List (x List ...) -> List
              Returns the list whose n-th element is the result of applying the procedure to the n-th elements of all
              the provided lists, in order of their appearances in the arguments list. The length of the returned list
              is the same as the shortest provided list. Providing more lists than the lambda requires will result in
              the extraneous lists (that appear at the end of the arguments list) being ignored. WARNING: program will
              crash if fewer lists are provided than the lambda requires.
       append : List (x List ...) x Any -> List or ImproperList
              Appends the first (n - 1) provided lists in order, then appends the last object. If the last object is a
              list, then the result will be a list; otherwise, it will be an improper list. For example, 
              (append '(1 2) '(3 4) '(5 6)) will return '(1 2 3 4 5 6), and (append '(1 2) '(3 4) 5) will return 
              '(1 2 3 4 . 5).
        foldl : Proc x Any x List (x List ...) -> Any
              Behaves like Racket's foldl function. WARNING: program will crash if fewer lists are provided than the 
              lambda requires.
        foldr : Proc x Any x List (x List ...) -> Any
              Behaves like Racket's foldr function. WARNING: program will crash if fewer lists are provided than the 
              lambda requires.
      vector? : Any -> Bool
              Returns #t if the argument is a vector, false otherwise.
       vector : Any ... -> Vector
              Returns a fixed-length vector containing the arguments, in order of appearance.
  make-vector : Int x Any -> Vector
              (make-vector n x) returns a vector containing n copies of x.
   vector-ref : Vector x Int -> Any
              (vector-ref v n) returns the object at index n. Returns '(null) if n is out of bounds.
  vector-set! : Vector x Int x Any -> Void
              (vector-set! v n x) mutates v by setting the object at position n to x. Nothing happens if n is out of
              bounds.
vector-length : Vector -> Int
              Returns the length of the provided vector.
        list? : Any -> Bool
              Returns #t if argument is a list, #f otherwise.
        void? : Any -> Bool
              Returns #t if argument is void, #f otherwise.
     promise? : Any -> Bool
              Returns #t if argument is a promise, #f otherwise.
   procedure? : Any -> Bool
              Returns #t if argument is a procedure, #f otherwise.
     integer? : Any -> Bool
              Returns #t if argument is an integer, #f otherwise.
      number? : Any -> Bool
              Identical to integer?
         void : <None> -> Void
              Returns void.
        print : Any -> Void
              Prints the provided argument. See prim_print() in header.cpp to see what the formatting looks like.
         halt : Any -> Void
              Prints the provided argument, then terminates the program with exit code 0.
          eq? : Any x Any -> Bool
              Returns #t if the two arguments are physically equal (i.e., behaves like == in Java) and #f otherwise.
         eqv? : Any x Any -> Bool
              Identical to eq?. This should be changed one day.
       equal? : Any x Any -> Bool
              Identical to eqv?. This should be changed one day.
          not : Any -> Bool
              Returns #t if argument evaluates to #f; #t otherwise.

------------------
| Runtime errors |
------------------

The following runtime errors were identified and had measures implemented to protect against them.
I had to change the testing infrastructure a couple of times to get eval-top-level to behave 
appropriately. Apologies for only having one dedicated test file in most cases.
    1) Division by zero. Fixed by adding a check in header.cpp and halting with an error message if b = 0.
       Test file: divide-by-zero.scm.
    2) Memory cap. Fixed in header.cpp by keeping track of the amount of memory allocated and terminating if it 
       goes over the 256 MB limit. Test file: memory-cap.scm.
    3) Applying non-function value. Wrote a assert_is_clo() function that ensures that an object is a closure
       before calling the function associated with it, and inserted it appropriately into the generated LLVM.
       Test files: apply-non-function-0.scm and apply-non-function-1.scm
    4) Uncaught runtime exception. Surrounded the output of eval-top-level with a guard that prints an error message 
       if an uncaught exception is raised. Test file: no-guard-exception.scm.
    5) Sadly, I didn't have time to do a fifth one. :-(
    
-----------------------------------
| Regarding the new feature, etc. |
-----------------------------------

Sadly, I procrastinated too much on this project and as a result, I didn't have time to implement hashes and sets 
like I wanted to. The very beginnings of my work on hashes can be found in commented lines in simplify-ir (utils.rkt)
and the bottom of header.cpp, but they are completely non-functional. I feel genuinely bad about this, but there's 
nothing I can do about it at this point. Maybe one day I'll decide to implement it, in which case you'll be able to
see it on the GitHub repository.

