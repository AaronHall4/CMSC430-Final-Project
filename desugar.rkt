#lang racket

; by Aaron Hall

(provide desugar)
(require "utils.rkt")
(require (only-in rnrs letrec* cond case if when unless raise guard))

; Input Language:
;--------------------------------------
; e ::= (letrec* ([x e] ...) e)
;     | (letrec ([x e] ...) e)
;     | (let* ([x e] ...) e)
;     | (let ([x e] ...) e)
;     | (let x ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (lambda (x ...+ . x) e)
;     | (dynamic-wind e e e)
;     | (guard (x cond-clause ...) e)
;     | (raise e)
;     | (delay e)
;     | (force e)
;     | (and e ...)
;     | (or e ...)
;     | (cond cond-clause ...)
;     | (case e case-clause ...)
;     | (if e e e)
;     | (when e e)
;     | (unless e e)
;     | (set! x e)
;     | (begin e ...+)
;     | (call/cc e)
;     | (apply e e)
;     | (e e ...)
;     | x
;     | op
;     | (quote dat)
;  
; cond-clause ::= (e) | (e e) | (else e)  ; in all test cases
; case-clause ::= ((dat ...) e) | (else e)  ; else clauses always come last
; dat is a datum satisfying datum? from utils.rkt
; x is a variable (satisfies symbol?)
; op is a symbol satisfying prim? from utils.rkt (if not otherwise in scope)
; op ::= promise? | null? | cons | car | + | ...  (see utils.rkt)

(define (cond-clause? e)
  (match e
    [`(,e0) #t]
    [`(else ,e0) #t]
    [`(,e0 ,e1) #t]
    [else #f]))

(define (case-clause? e)
  (match e
    [`((,(? datum? dat) ...) ,e0) #t]
    [`(else ,e0) #t]
    [else #f]))

(define (desugar-aux e)
  (define (desugar-aux-list es)
    (map (lambda (e) (desugar-aux e)) es))
  
  (match e
    ; letrec forms
    [`(letrec* ([,(? symbol? xs) ,e0s] ...) ,e1)
     (define initlist (map (lambda (x) `[,x '%undefined]) xs))
     (define beginlist (map (lambda (x e) `(set! ,x ,e)) xs e0s))
     (desugar-aux `(let ,initlist (begin . ,(append beginlist `(,e1)))))]
    [`(letrec ([,(? symbol? xs) ,e0s] ...) ,e1)
     (define initlist (map (lambda (x) `[,x '%undefined]) xs))
     (define dummylist (map (lambda (x) (gensym x)) xs))
     (define letlist (map (lambda (t e) `[,t ,e]) dummylist e0s))
     (define beginlist (map (lambda (x t) `(set! ,x ,t)) xs dummylist))
     (desugar-aux `(let ,initlist (let ,letlist (begin . ,(append beginlist `(,e1))))))]

    ; let forms
    [`(let* ([,(? symbol? xs) ,e0s] ...) ,e1)
     (define (gen-lets xs es)
       (if (null? es)
           (desugar-aux e1)
           `(let ([,(car xs) ,(desugar-aux (car es))]) ,(gen-lets (cdr xs) (cdr es)))))
     (gen-lets xs e0s)]
    [`(let ([,(? symbol? xs) ,e0s] ...) ,e1)
     `(let ,(map (lambda (x e) `[,x ,e]) xs (desugar-aux-list e0s)) ,(desugar-aux e1))]
    [`(let ,(? symbol? fun) ([,(? symbol? xs) ,e0s] ...) ,e1)
     (desugar-aux `(letrec ([,fun (lambda ,xs ,e1)]) (,fun . ,e0s)))]

    ; Lambda expressions
    [`(lambda (,(? symbol? xs) ...) ,e)
     `(lambda ,xs ,(desugar-aux e))]
    [`(lambda (,(? symbol? x0s) ..1 . ,(? symbol? x1)) ,e)
     (define (repeat-cdr n x)
       (if (= n 0) x `(prim cdr ,(repeat-cdr (sub1 n) x))))
     (define (repeat-cdr-list lst x)
       (map (lambda (e n) `[,e (prim car ,(repeat-cdr n x))]) lst (build-list (length lst) values)))
     `(lambda t (let ,(append (repeat-cdr-list x0s 't) `((,x1 ,(repeat-cdr (length x0s) 't)))) ,(desugar-aux e)))]
    [`(lambda ,(? symbol? x) ,e)
     `(lambda ,x ,(desugar-aux e))]

    ; Continuations and exceptions
    [`(call/cc ,e)
     `(call/cc ,(desugar-aux `(lambda (k)
                                (,e (let ([k-stack %wind-stack])
                                      (lambda (x)
                                        (begin (%do-wind k-stack)
                                               (k x))))))))]
    [`(dynamic-wind ,entry ,body ,exit)
     (desugar-aux `(%dynamic-wind ,entry ,body ,exit))]
    [`(guard (,(? symbol? x) ,(? cond-clause? ccs) ...) ,e)
     (desugar-aux `(let ([cc (call/cc (lambda (k) k))])
                     (if (cons? cc)
                         (let ([,x (car cc)])
                           ,(cons 'cond (append ccs `((else (raise ,x))))))
                         (let ([old-handler %exception-handler])
                           (dynamic-wind
                            (lambda () (set! %exception-handler cc))
                            (lambda () ,e)
                            (lambda () (set! %exception-handler old-handler)))))))]
    [`(raise ,e)
     (desugar-aux `(%exception-handler (cons ,e '())))]

    ; Promises
    [`(delay ,e0)
     (desugar-aux `(vector '%promise-identifier-tag '#f '(void) (lambda () ,e0)))]
    [`(force ,e0)
     (define forcevar (gensym 'forceexpr))
     (desugar-aux `(let ([,forcevar ,e0])
                     (if (promise? ,forcevar)
                         (if (vector-ref ,forcevar '1)
                             (vector-ref ,forcevar '2)
                             (begin
                               (vector-set! ,forcevar '1 '#t)
                               (vector-set! ,forcevar '2 ((vector-ref ,forcevar '3)))
                               (vector-ref ,forcevar '2)))
                         ,forcevar)))]
    [`(promise? ,p)
     (desugar-aux `(%is-promise? ,p))]
    [`(apply promise? ,e)
     (desugar-aux `(apply %is-promise? ,e))]

    ; Boolean operators
    ['(and) ''#t]
    [`(and ,e0s ...)
     (define (gen-if es)
       (match es
         ['() '#t]
         [(cons e '()) e]
         [(cons e rest)
          (define andifvar (gensym 'andifvar))
          `(let ([,andifvar ,e]) (if ,andifvar ,(gen-if rest) ,andifvar))]))
     (desugar-aux (gen-if e0s))]
    ['(or) ''#f]
    [`(or ,e0s ...)
     (define (gen-if es)
       (match es
         ['() '#f]
         [(cons e '()) e]
         [(cons e rest)
          (define orifvar (gensym 'orifvar))
          `(let ([,orifvar ,e]) (if ,orifvar ,orifvar ,(gen-if rest)))]))
     (desugar-aux (gen-if e0s))]

    ; Conditional operators
    [`(cond ,(? cond-clause? cond-clauses) ...)
     (define (gen-if cond-clauses)
       (define cond-clause (if (pair? cond-clauses) (car cond-clauses) '()))
       (match cond-clause
         ['() '(void)]
         [`(,e) e]
         [`(else ,e) e]
         [`(,e0 ,e1)
          `(if ,e0 ,e1 ,(gen-if (cdr cond-clauses)))]))
     (desugar-aux (gen-if cond-clauses))]
    [`(case ,e0 ,(? case-clause? case-clauses) ...)
     (define e0var (gensym 'casevar))
     (define (gen-if case-clauses)
       (define case-clause (if (pair? case-clauses) (car case-clauses) '()))
       (match case-clause
         ['() '(void)]
         [`(else ,e) e]
         [`((,(? datum? dat) ...) ,e)
          `(if (memv ,e0var (quote ,dat)) ,e ,(gen-if (cdr case-clauses)))]))
     (desugar-aux `(let ([,e0var ,e0]) ,(gen-if case-clauses)))]
    [`(if ,e0 ,e1 ,e2)
     `(if ,(desugar-aux e0) ,(desugar-aux e1) ,(desugar-aux e2))]
    [`(when ,e0 ,e1)
     `(if ,(desugar-aux e0) ,(desugar-aux e1) (prim void))]
    [`(unless ,e0 ,e1)
     `(if ,(desugar-aux e0) (prim void) ,(desugar-aux e1))]

    ; set!
    [`(set! ,(? symbol? x) ,e)
     `(set! ,x ,(desugar-aux e))]

    ; begin
    [`(begin ,es ..1)
     (define beginexplist
       (map (lambda (x) `(,(gensym 'beginexp) ,x)) es))
     (desugar-aux `(let* ,beginexplist ,(car (last beginexplist))))]

    ; Symbolic stuff
    ['promise?
     `(lambda (x) (%is-promise? x))]
    [(? prim? prim)
     `(lambda args (apply-prim ,prim args))]
    [(? symbol? x)
     x]
    [`(quote ,(? datum? dat))
     `(quote ,dat)]

    ; Application (tagged and untagged)
    [`(apply ,(? prim? prim) ,e1)
     `(apply-prim ,prim ,(desugar-aux e1))]
    [`(apply ,e0 ,e1)
     `(apply ,(desugar-aux e0) ,(desugar-aux e1))]
    [`(,(? prim? prim) ,e1s ...)
     `(prim ,prim . ,(desugar-aux-list e1s))]
    [`(,e0 ,e1s ...)
     `(,(desugar-aux e0) . ,(desugar-aux-list e1s))]

    ; Not in input language
    [else '()]))


(define (desugar e)
  (desugar-aux
   `(let* ([%is-promise? (lambda (p)
                           (and (vector? p)
                                (equal? (vector-length p) '4)
                                (equal? (vector-ref p '0) '%promise-identifier-tag)))]
           [%wind-stack '()]
           [common-tail (lambda (x y)
                          (let ((lx (length x))
                                (ly (length y)))
                            (let loop ([x (if (> lx ly) (drop x (- lx ly)) x)]
                                       [y (if (> ly lx) (drop y (- ly lx)) y)])
                              (if (eq? x y)
                                  x
                                  (loop (cdr x) (cdr y))))))]
           [%do-wind (lambda (new)
                       (unless (eq? new %wind-stack) 
                         (let ([tail (common-tail new %wind-stack)])
                           (begin
                             (let f ((l %wind-stack))
                               (unless (eq? l tail)
                                 (begin
                                   (set! %wind-stack (cdr l))
                                   ((cdr (car l)))
                                   (f (cdr l)))))
                             (let f ([l new])
                               (unless (eq? l tail)
                                 (begin
                                   (f (cdr l))
                                   ((car (car l)))
                                   (set! %wind-stack l))))))))]
           [%dynamic-wind (lambda (pre body post)
                            (begin
                              (pre)
                              (set! %wind-stack (cons (cons pre post) %wind-stack))
                              (let ([v (body)])
                                (begin
                                  (set! %wind-stack (cdr %wind-stack))
                                  (post)
                                  v))))]
           [%exception-handler '()])
      ,e)))


; I, Aaron Hall, pledge on my honor that I have not given or 
; received any unauthorized assistance on this project.
