#lang racket

(require "top-level.rkt")
(require "desugar.rkt")
(require "cps.rkt")
(require "closure-convert.rkt")
(require "utils.rkt")

(provide compile-to-cps
         compile-to-cc
         compile-to-llvm
         compile-to-binary
         test-compiler)

; Various paths used for compilation.
(define project-path (current-directory))
(define libgc-path
  (path->string
   (build-path project-path "lib" "local" "lib" "libgc.a")))
(define gc-include-path
  (path->string
   (build-path project-path "lib" "local" "include")))
(define clang++-path
  (let ([clang++-path-submit-server "/opt/llvm-3.9.0/bin/clang++"])
    (if (file-exists? clang++-path-submit-server)
        clang++-path-submit-server
        "clang++")))

(define (compile-to-cps prog)
  (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar (top-level prog))))))))

(define (compile-to-cc prog)
  (closure-convert (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar (top-level prog)))))))))

; Compile a Scheme program from the top level all the way down to LLVM.
(define (compile-to-llvm prog)
  (proc->llvm (closure-convert (cps-convert (anf-convert (alphatize (assignment-convert (simplify-ir (desugar (top-level prog))))))))))

; Compile a Scheme program from the top level all the way down to an executable binary.
(define recent-header #f)
(define (compile-to-binary prog progname [save-llvm #f])
  (define prog-llvm (compile-to-llvm prog))
  (when (or (not (file-exists? "header.ll")) (not recent-header))
    (set! recent-header #t)
    (system (string-append clang++-path " header.cpp " " -I " gc-include-path " -S -emit-llvm -o header.ll")))
  (define header-llvm (read-string 1000000 (open-input-file "header.ll" #:mode 'text)))
  (define combined-llvm (string-append header-llvm "\n\n;;;;;;\n\n" prog-llvm))
  (display combined-llvm (open-output-file (string-append progname ".ll") #:exists 'replace))
  (system (string-append clang++-path " " progname ".ll " libgc-path " -I " gc-include-path " -lpthread -o " progname))
  (unless save-llvm
    (delete-file (string-append progname ".ll"))))

; Tests this compiler, to ensure that the top-level and LLVM versions of the program are the same.
(define (test-compiler prog)
  (define tl-val (eval-top-level prog))
  (define prog-llvm (compile-to-llvm prog))
  (define llvm-val (eval-llvm prog-llvm))
  (if (equal? tl-val llvm-val)
      #t
      (begin
        (display (format "test-compiler: two different values (~a and ~a) before and after compilation.\n"
                         tl-val llvm-val))
        #f)))