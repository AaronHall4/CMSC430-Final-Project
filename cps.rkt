#lang racket

(provide assignment-convert
         alphatize
         anf-convert
         cps-convert)

(require "desugar.rkt")
(require "utils.rkt")


; The output of assignment 2:
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (set! x e)
;     | (call/cc e)
;     | x
;     | (quote dat)

(define (find-mutated-vars e)
  ; The mutated variables of a list of expression is just the union of
  ; the set of mutated variables of each expression.
  (define (find-mutated-vars-lst es)
    (foldl set-union (set) (map find-mutated-vars es)))

  ; Recursively construct the set of mutated variables.
  (match e
    [`(let ([,(? symbol? xs) ,e0s] ...) ,e1)
     (set-union (find-mutated-vars-lst e0s) (find-mutated-vars e1))]
    [`(lambda ,(? symbol? x) ,e)
     (find-mutated-vars e)]
    [`(lambda (,(? symbol? xs) ...) ,e)
     (find-mutated-vars e)]
    [`(apply ,e0 ,e1)
     (set-union (find-mutated-vars e0) (find-mutated-vars e1))]
    [`(prim ,(? prim? op) ,es ...)
     (find-mutated-vars-lst es)]
    [`(apply-prim ,(? prim? op) ,e)
     (find-mutated-vars e)]
    [`(if ,e0 ,e1 ,e2)
     (set-union (find-mutated-vars e0)
                (find-mutated-vars e1)
                (find-mutated-vars e2))]
    [`(set! ,(? symbol? x) ,e)
     (set-add (find-mutated-vars e) x)]
    [`(call/cc ,e)
     (find-mutated-vars e)]
    [(? symbol? x)
     (set)]
    [`(quote ,(? datum? dat))
     (set)]
    [`(,e0 ,e1s ...)
     (set-union (find-mutated-vars e0) (find-mutated-vars-lst e1s))]))

(define (assignment-convert e)
  ; Find all mutated variables.
  (define mutated-vars (find-mutated-vars e))

  ; Box all mutated variables.
  (define (boxify e)
    (match e
      [`(let ([,(? symbol? xs) ,e0s] ...) ,e1)
       (define boxed-lst (map (lambda (x e)
                                (if (set-member? mutated-vars x)
                                    `[,x (prim make-vector '1 ,(boxify e))]
                                    `[,x ,(boxify e)]))
                              xs e0s))
       `(let ,boxed-lst ,(boxify e1))]
      [`(lambda ,(? symbol? x) ,e)
       (if (set-member? mutated-vars x)
           `(lambda ,x (let ([,x (prim make-vector '1 ,x)]) ,(boxify e)))
           `(lambda ,x ,(boxify e)))]
      [`(lambda (,(? symbol? xs) ...) ,e)
       (define mutable-params (set-intersect mutated-vars (list->set xs)))
       (define let-params (map (lambda (x) `[,x (prim make-vector '1 ,x)]) (set->list mutable-params)))
       (if (set-empty? mutable-params)
           `(lambda ,xs ,(boxify e))
           `(lambda ,xs (let ,let-params ,(boxify e))))]
      [`(apply ,e0 ,e1)
       `(apply ,(boxify e0) ,(boxify e1))]
      [`(prim ,(? prim? op) ,es ...)
       `(prim ,op ,@(map boxify es))]
      [`(apply-prim ,(? prim? op) ,e)
       `(apply-prim ,op ,(boxify e))]
      [`(if ,e0 ,e1 ,e2)
       `(if ,(boxify e0) ,(boxify e1) ,(boxify e2))]
      [`(set! ,x ,e0)
       `(prim vector-set! ,x '0 ,(boxify e0))]
      [`(call/cc ,e)
       `(call/cc ,(boxify e))]
      [(? symbol? x)
       (if (set-member? mutated-vars x)
           `(prim vector-ref ,x '0)
           x)]
      [`(quote ,(? datum? dat))
       `(quote ,dat)]
      [`(,e0 ,e1s ...)
       `(,(boxify e0) ,@(map boxify e1s))]))
  (boxify e))


; assignment-convert => 

;;; set! is removed and replaced with vector-set!
; e ::= (let ([x e] ...) e)
;     | (lambda (x ...) e)
;     | (lambda x e)
;     | (apply e e)
;     | (e e ...)
;     | (prim op e ...)
;     | (apply-prim op e)
;     | (if e e e)
;     | (call/cc e)
;     | x
;     | (quote dat)

; alphatize both takes and produces this language as well

(define (alphatize e)
  ; Rename all variables, keeping track of the current environment to ensure that
  ; everything is renamed appropriately.
  (define ((rename env) e)
    (match e
      [`(let ([,(? symbol? xs) ,e0s] ...) ,e1)
       (define xs+ (map gensym xs))
       (define e0s+ (map (rename env) e0s))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       (define new-lst (map (lambda (x e) `[,x ,e]) xs+ e0s+))
       `(let ,new-lst ,((rename env+) e1))]
      [`(lambda ,(? symbol? x) ,e)
       (define x+ (gensym x))
       (define env+ (hash-set env x x+))
       `(lambda ,x+ ,((rename env+) e))]
      [`(lambda (,(? symbol? xs) ...) ,e)
       (define xs+ (map gensym xs))
       (define env+ (foldl (lambda (x x+ env) (hash-set env x x+)) env xs xs+))
       `(lambda ,xs+ ,((rename env+) e))]
      [`(apply ,e0 ,e1)
       `(apply ,((rename env) e0) ,((rename env) e1))]
      [`(prim ,(? prim? op) ,es ...)
       `(prim ,op ,@(map (rename env) es))]
      [`(apply-prim ,(? prim? op) ,e)
       `(apply-prim ,op ,((rename env) e))]
      [`(if ,e0 ,e1 ,e2)
       `(if ,((rename env) e0) ,((rename env) e1) ,((rename env) e2))]
      [`(call/cc ,e)
       `(call/cc ,((rename env) e))]
      [(? symbol? x)
       (hash-ref env x)]
      [`(quote ,(? datum? dat))
       `(quote ,dat)]
      [`(,e0 ,e1s ...)
       `(,((rename env) e0) ,@(map (rename env) e1s))]))
  ((rename (hash)) e))


(define (anf-convert e)
  ; Normalize an atomic expression.
  (define (normalize-ae e k)
    (normalize e (lambda (anf)
                   (match anf
                     [(? symbol? x)
                      (k x)]
                     [`(lambda ,(? symbol? x) ,e)
                      (k `(lambda ,x ,e))]
                     [`(lambda (,(? symbol? xs) ...) ,e)
                      (k `(lambda ,xs ,e))]
                     [`(quote ,(? datum? dat))
                      (k `(quote ,dat))]
                     [else
                      (define abv (gensym 'anfbindvar))
                      `(let ([,abv ,anf])
                         ,(k abv))]))))

  ; Normalize a list of atomic expressions.
  (define (normalize-aes es k)
    (if (null? es)
        (k '())
        (normalize-ae (car es) (lambda (ae)
                                 (normalize-aes (cdr es)
                                                (lambda (aes)
                                                  (k `(,ae ,@aes))))))))

  ; Normalize a complex expression.
  (define (normalize e k)
    (match e
      [`(let ([,(? symbol? xs) ,e0s] ...) ,e1)
       (define (unravel-let xs e0s)
         (if (null? xs)
             (anf-convert e1)
             `(let ([,(car xs) ,(anf-convert (car e0s))])
                ,(unravel-let (cdr xs) (cdr e0s)))))
       (k (unravel-let xs e0s))]
      [`(lambda ,(? symbol? x) ,e)
       (k `(lambda ,x ,(anf-convert e)))]
      [`(lambda (,(? symbol? xs) ...) ,e)
       (k `(lambda ,xs ,(anf-convert e)))]
      [`(apply ,e0 ,e1)
       (normalize-aes (list e0 e1) (lambda (aes)
                                     (k `(apply ,@aes))))]
      [`(prim ,(? prim? op) ,es ...)
       (normalize-aes es (lambda (aes)
                           (k `(prim ,op ,@aes))))]
      [`(apply-prim ,(? prim? op) ,e)
       (normalize-ae e (lambda (ae)
                         (k `(apply-prim ,op ,ae))))]
      [`(if ,e0 ,e1 ,e2)
       (normalize-ae e0 (lambda (ae)
                          (k `(if ,ae ,(anf-convert e1) ,(anf-convert e2)))))]
      [`(call/cc ,e)
       (normalize-ae e (lambda (ae)
                         (k `(call/cc ,ae))))]
      [(? symbol? x)
       (k x)]
      [`(quote ,(? datum? dat))
       (k `(quote ,dat))]
      [`(,e0 ,e1s ...)
       (normalize-aes (cons e0 e1s) k)]))
  
  (normalize e (lambda (x) x)))


; anf-convert =>

; e ::= (let ([x e]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (prim op ae ...)
;     | (apply-prim op ae)
;     | (if ae e e)
;     | (call/cc ae)
;     | ae
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


(define (cps-convert e)
  ; CPS-convert an atomic expression (one that cannot extend the stack).
  (define (T-ae ae)
    (match ae
      [(? symbol? x)
       x]
      [`(lambda ,(? symbol? x) ,e)
       (define k (gensym 'kont))
       `(lambda ,x (let ([,k (prim car ,x)])
                     (let ([,x (prim cdr ,x)])
                       ,(T-e e k))))]
      [`(lambda (,(? symbol? xs) ...) ,e)
       (define k (gensym 'kont))
       `(lambda ,(cons k xs) ,(T-e e k))]
      [`(quote ,(? datum? dat))
       `(quote ,dat)]
      [else
       ae]))

  ; CPS-convert a complex expression (one that extends the stack).
  (define (T-e e cae)
    (match e
      [`(let ([,(? symbol? x) ,e0]) ,e1)
       (define _x (gensym '_))
       (T-e e0 `(lambda (,_x ,x) ,(T-e e1 cae)))]
      [`(lambda . ,rest)
       `(,cae '0 ,(T-ae e))]
      [`(apply ,ae0 ,ae1)
       (define lav (gensym 'letapplyvar))
       `(let ([,lav (prim cons ,cae ,(T-ae ae1))]) (apply ,(T-ae ae0) ,lav))]
      [`(if ,ae ,e0 ,e1)
       `(if ,(T-ae ae) ,(T-e e0 cae) ,(T-e e1 cae))]
      [`(prim ,(? prim? op) ,aes ...)
       (define lpv (gensym 'letprimvar))
       `(let ([,lpv (prim ,op ,@(map T-ae aes))]) (,cae ,lpv ,lpv))]
      [`(apply-prim ,(? prim? op) ,ae)
       (define lapv (gensym 'letapplyprimvar))
       `(let ([,lapv (apply-prim ,op ,(T-ae ae))]) (,cae ,lapv ,lapv))]
      [(? symbol? x)
       `(,cae ,x ,x)]
      [`(call/cc ,ae)
       `(,(T-ae ae) ,cae ,cae)]
      [`(quote ,(? datum? dat))
       (define lqv (gensym 'letquotevar))
       `(let ([,lqv (quote ,dat)]) (,cae ,lqv ,lqv))]
      [`(,aef ,aes ...)
       `(,(T-ae aef) ,cae ,@(map T-ae aes))]))

  ; Kick it off with an initial continuation (lambda (k x) ..)
  ; Its continuation k is never used because first the prim halt is applied on x.
  (T-e e '(lambda (k x) (let ([_1 (prim halt x)]) (k x)))))


; cps-convert => 

; e ::= (let ([x (apply-prim op ae)]) e)
;     | (let ([x (prim op ae ...)]) e)
;     | (let ([x (lambda x e)]) e)
;     | (let ([x (lambda (x ...) e)]) e)
;     | (let ([x (quote dat)]) e)
;     | (apply ae ae)
;     | (ae ae ...)
;     | (if ae e e)
; ae ::= (lambda (x ...) e)
;      | (lambda x e)
;      | x
;      | (quote dat)


