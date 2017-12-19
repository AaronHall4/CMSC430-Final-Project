#lang racket

(provide top-level)

(require "utils.rkt")

; By Aaron Hall

(define (get-test-input test)
  (with-input-from-file (string->path (string-append "tests/public/" test ".scm")) read-begin #:mode 'text))

(define total-output '())
(define println (lambda lst
                  (define (insert-at-end lst x)
                    (if (null? lst)
                        (cons x '())
                        (cons (car lst) (insert-at-end (cdr lst) x))))
                  (define lst+ (insert-at-end lst "\n"))
                  (map display lst+)
                  (set! total-output (insert-at-end total-output lst+))
                  total-output))

(define (zip-args xs es)
  (map (lambda (x e) `[,x ,e]) xs es))

(define (wrap-elist es)
  (if (> (length es) 1)
      (cons 'begin es)
      (car es)))

(define (convert-cond-clauses ccs)
  (map (lambda (cc) (match cc
                      [`(,e) `(,(top-level-aux e))]
                      [`(,e0 ,e1 ,e2s ...)
                       (define begin-e1s (wrap-elist (cons e1 e2s)))
                       `(,(top-level-aux e0) ,(top-level-aux begin-e1s))]
                      [`(else ,e0 ,e1s ...)
                       (define begin-e0s (wrap-elist (cons e0 e1s)))
                       `(else ,(top-level-aux begin-e0s))]
                      [else (raise "Error: invalid cond clause")])) ccs))

(define (convert-case-clauses ccs)
  (map (lambda (cc) (match cc
                      [`((,(? datum? dats) ...) ,e0 ,e1s ...)
                       (define begin-e0s (wrap-elist (cons e0 e1s)))
                       `(,dats ,(top-level-aux begin-e0s))]
                      [`(else ,e0 ,e1s ...)
                       (define begin-e0s (wrap-elist (cons e0 e1s)))
                       `(else ,(top-level-aux begin-e0s))]
                      [else (raise "Error: invalid case clause")])) ccs))

(define (flatten-begins begin-es)
  (if (null? begin-es)
      '()
      (let ([e0 (car begin-es)])
        (match e0
          [`(begin ,es ..1)
           (append (flatten-begins es) (flatten-begins (cdr begin-es)))]
          [else
           (cons e0 (flatten-begins (cdr begin-es)))]))))

(define (define? exp)
  (match exp
    [`(define ,(? symbol? x) ,e)
     #t]
    [`(define (,(? symbol? f) ,(? symbol? x0s) ... (,(? symbol? defxs) ,defes) ...) ,es ..1)
     #t]
    [`(define (,(? symbol? f) ,(? symbol? x0s) ... . ,(? symbol? x1)) ,es ..1)
     #t]
    [else
     #f]))

(define (convert-defines begin-es)
  ; Converts all the defines in the begin into one letrec*, setting its body to be
  ; the remaining statements.
  (define (create-one-letrec* definelist remaining-es)
    (define (define-to-binding def)
      (match def
        [`(define ,(? symbol? x) ,e)
         `[,x ,e]]
        [`(define (,(? symbol? f) ,(? symbol? x0s) ... (,(? symbol? defxs) ,defes) ...) ,es ..1)
         (define deflist (zip-args defxs defes))
         `[,f (lambda ,(append x0s deflist) ,@es)]]
        [`(define (,(? symbol? f) ,(? symbol? x0s) ... . ,(? symbol? x1)) ,es ..1)
         `[,f (lambda (,@x0s . ,x1) ,@es)]]
        [else
         (raise "Error: expected define")]))
    `(letrec* ,(map define-to-binding definelist) ,(wrap-elist remaining-es)))
  (define defines (filter define? begin-es))
  (define others (filter (lambda (e) (not (define? e))) begin-es))
  (if (null? defines)
      (wrap-elist others)
      (create-one-letrec* defines others)))

; Converts a match case into a cond-clause
(define (convert-match-case e0 mc)
  (match-define `[,pattern ,e1s ..1] mc)
  (match pattern
    ['else
     `[else ,@e1s]]
    [(? natural? nat)
     `[(equal? ,e0 ,nat) ,@e1s]]
    [(? string? str)
     `[(equal? ,e0 ,str) ,@e1s]]
    [#t
     `[(equal? ,e0 #t) ,@e1s]]
    [#f
     `[(equal? ,e0 #f) ,@e1s]]
    [`(quote ,dat)
     `[(equal? ,e0 (quote ,dat)) ,@e1s]]
    [(? symbol? x)
     `[#t (let ([,x ,e0]) ,@e1s)]]
    [`(? ,e2 ,pat2)
     (match-define `[,pat2cond ,e1s+ ..1] (convert-match-case e0 `[,pat2 ,@e1s]))
     `[(and (,e2 ,e0) ,pat2cond) ,@e1s+]]
    [`(cons ,pat1 ,pat2)
     (match-define `[,pat2cond ,pat2e1s ..1] (convert-match-case `(cdr ,e0) `[,pat2 ,@e1s]))
     (match-define `[,pat1cond ,pat1e1s ..1] (convert-match-case `(car ,e0) `[,pat1 ,@pat2e1s]))
     `[(and (cons? ,e0) ,pat1cond ,pat2cond) ,@pat1e1s]]
    [`(quasiquote ,qqpat)
     (define expanded-qqpat (convert-quasiquote qqpat 1 0))
     (convert-match-case e0 `[,expanded-qqpat ,@e1s])]))

(define (convert-quasiquote qq nqqs nuqs)
  (define (make-cons lst end)
    (if (null? lst)
        end
        `(cons ,(car lst) ,(make-cons (cdr lst) end))))

  (when (> nuqs nqqs)
    (raise "Error: too many unquotes"))
  (match qq
    [(list 'unquote innerqq) ; ,e
     (match innerqq
       [(list 'unquote _) ; ,,e
        `(cons ,''unquote (cons ,(convert-quasiquote innerqq nqqs (+ 1 nuqs)) '()))]
       [`(quasiquote ,inner^2qq) ; ,`e
        (if (= nqqs (+ 1 nuqs))
            ; Equal # of quasiquotes and unquotes, so unquote
            (convert-quasiquote inner^2qq 1 0)
            ; More quasiquotes than unquotes, so wrap in literal unquote and continue
            `(cons ,''unquote (cons ,(convert-quasiquote innerqq (+ 1 nqqs) (+ 2 nuqs)) '())))]
       [(? datum? dat) ; ,dat
        (if (= nqqs (+ 1 nuqs))
            dat
            `(cons ,''unquote (cons ,(convert-quasiquote dat nqqs (+ 1 nuqs)) '())))]
       [e ; ,e for any other e
        (if (= nqqs (+ 1 nuqs))
            ; Equal # of quasiquotes and unquotes, so unquote
            e
            ; More quasiquotes than unquote, so wrap in literal unquote and continue
            `(cons ,''unquote (cons ,(convert-quasiquote e nqqs (+ 1 nuqs)) '())))])]
    [(list innerqqs ..1 'unquote e) ; `(qq ...+ . (unquote e))
     (make-cons (map (lambda (iqq) (convert-quasiquote iqq nqqs nuqs)) innerqqs)
                (convert-quasiquote (list 'unquote e) nqqs nuqs))]
    [(list-rest innerqq0s ... innerqq1) ; Anything else
     (if (null? innerqq0s)
         ; Only one object: `e
         (match qq
           [(? datum? dat)
            `(quote ,dat)])
         ; Either a list or an improper list.
         (match innerqq0s
           [`(quasiquote ,innerqq) ; ``e -> '`e
            `(cons ,''quasiquote (cons ,(convert-quasiquote innerqq (+ 1 nqqs) nuqs) '()))]
           [else
            (make-cons (map (lambda (iqq) (convert-quasiquote iqq nqqs nuqs)) innerqq0s)
                       (if (null? innerqq1)
                           ''() ; `(e e ...)
                           (convert-quasiquote innerqq1 nqqs nuqs)))]))])) ; `(e ... . e)

(define (top-level-aux e)
  (match e
    [`(define ,(? symbol? x) ,e)
     (raise "Error: unexpected define")]
    [`(define (,(? symbol? f) ,(? symbol? x0s) ... (,(? symbol? defxs) ,defes) ...) ,es ..1)
     (raise "Error: unexpected define")]
    [`(define (,(? symbol? f) ,(? symbol? x0s) ... . ,(? symbol? x1)) ,es ..1)
     (raise "Error: unexpected define")]

    [`(letrec* ([,(? symbol? xs) ,e0s] ...) ,e1s ..1)
     (define begin-e1s (wrap-elist e1s))
     (define begin-e1s+ (top-level-aux begin-e1s))
     (define e0s+ (map top-level-aux e0s))
     `(letrec* ,(zip-args xs e0s+) ,begin-e1s+)]
    [`(letrec ([,(? symbol? xs) ,e0s] ...) ,e1s ..1)
     (define begin-e1s (wrap-elist e1s))
     (define begin-e1s+ (top-level-aux begin-e1s))
     (define e0s+ (map top-level-aux e0s))
     `(letrec ,(zip-args xs e0s+) ,begin-e1s+)]
    [`(let* ([,(? symbol? xs) ,e0s] ...) ,e1s ..1)
     (define begin-e1s (wrap-elist e1s))
     (define begin-e1s+ (top-level-aux begin-e1s))
     (define e0s+ (map top-level-aux e0s))
     `(let* ,(zip-args xs e0s+) ,begin-e1s+)]
    [`(let ([,(? symbol? xs) ,e0s] ...) ,e1s ..1)
     (define begin-e1s (wrap-elist e1s))
     (define begin-e1s+ (top-level-aux begin-e1s))
     (define e0s+ (map top-level-aux e0s))
     `(let ,(zip-args xs e0s+) ,begin-e1s+)]
    [`(let ,(? symbol? x0) ([,(? symbol? x1s) ,e0s] ...) ,e1s ..1)
     (define begin-e1s (wrap-elist e1s))
     (define begin-e1s+ (top-level-aux begin-e1s))
     (define e0s+ (map top-level-aux e0s))
     `(let ,x0 ,(zip-args x1s e0s+) ,begin-e1s+)]

    [`(lambda (,(? symbol? x0s) ..1 . ,(? symbol? x1)) ,es ..1)
     (define begin-es (wrap-elist es))
     (define begin-es+ (top-level-aux begin-es))
     `(lambda (,@x0s . ,x1) ,begin-es+)]
    [`(lambda (,(? symbol? x0s) ... (,(? symbol? defxs) ,defes) ...) ,es ..1)
     (define begin-es (wrap-elist es))
     (define begin-es+ (top-level-aux begin-es))
     (define defvar (gensym 'defaults))
     (define (initdefs xlist elist)
       (if (null? xlist)
           ''()
           `(unless (null? ,defvar)
              (set! ,(car xlist) (car ,defvar))
              (set! ,defvar (cdr ,defvar))
              ,(initdefs (cdr xlist) (cdr elist)))))
     (if (null? defxs)
         `(lambda ,x0s ,begin-es+)
         (top-level-aux `(lambda (,@x0s . ,defvar)
                           (when (> (length ,defvar) ,(length defxs))
                             (raise "Error: too many arguments passed"))
                           (let ,(zip-args defxs defes)
                             ,(initdefs defxs defes)
                             ,@es))))]
    [`(lambda ,(? symbol? x) ,es ..1)
     (define begin-es (wrap-elist es))
     (define begin-es+ (top-level-aux begin-es))
     `(lambda ,x ,begin-es+)]

    [`(call/cc ,e)
     `(call/cc ,(top-level-aux e))]
    [`(dynamic-wind ,e0 ,e1 ,e2)
     `(dynamic-wind ,(top-level-aux e0) ,(top-level-aux e1) ,(top-level-aux e2))]
    [`(guard (,(? symbol? x) ,ccs ...) ,es ..1)
     (define converted-ccs (convert-cond-clauses ccs))
     (define begin-es (wrap-elist es))
     (define begin-es+ (top-level-aux begin-es))
     `(guard (,x ,@converted-ccs) ,begin-es+)]
    [`(raise ,e)
     `(raise ,(top-level-aux e))]

    [`(delay ,e)
     `(delay ,(top-level-aux e))]
    [`(force ,e)
     `(force ,(top-level-aux e))]

    [`(match ,e ,mcs ...)
     (define (is-else? mc)
       (match mc
         [`[else ,useless ...] #t]
         [else #f]))
     (define matchvar (gensym 'matchvar))
     (define cond-clauses (map (lambda (mc) (convert-match-case matchvar mc)) mcs))
     (define cond-clauses+ (if (member #t (map is-else? mcs))
                               cond-clauses
                               (append cond-clauses (list '[else (raise "Error: no match")]))))
     (top-level-aux `(let ([,matchvar ,e]) (cond ,@cond-clauses+)))]

    [`(and ,es ...)
     `(and ,@(map top-level-aux es))]
    [`(or ,es)
     `(or ,@(map top-level-aux es))]

    [`(cond ,ccs ...)
     `(cond ,@(convert-cond-clauses ccs))]
    [`(case ,e ,ccs ...)
     `(case ,(top-level-aux e) ,@(convert-case-clauses ccs))]

    [`(if ,e0 ,e1 ,e2)
     `(if ,(top-level-aux e0) ,(top-level-aux e1) ,(top-level-aux e2))]
    [`(when ,e0 ,e1s ..1)
     (define begin-e1s (wrap-elist e1s))
     (define begin-e1s+ (top-level-aux begin-e1s))
     `(when ,(top-level-aux e0) ,begin-e1s+)]
    [`(unless ,e0 ,e1s ..1)
     (define begin-e1s (wrap-elist e1s))
     (define begin-e1s+ (top-level-aux begin-e1s))
     `(unless ,(top-level-aux e0) ,begin-e1s+)]

    [`(set! ,(? symbol? x) ,e)
     `(set! ,x ,(top-level-aux e))]

    [`(begin ,es ..1)
     (define es+ (flatten-begins es))
     (define es++ (convert-defines es+))
     (if (equal? (car es++) 'begin)
         `(begin ,@(map top-level-aux (cdr es++)))
         (top-level-aux es++))]

    [(? symbol? x)
     x]
    [(? prim? op)
     op]

    [`(quasiquote ,qq)
     (top-level-aux (convert-quasiquote qq 1 0))]
    [`(quote ,(? datum? dat))
     `(quote ,dat)]

    [(? natural? nat)
     `(quote ,nat)]
    [(? string? str)
     `(quote ,str)]
    [#t
     `(quote #t)]
    [#f
     `(quote #f)]

    [`(apply ,e0 ,e1)
     `(apply ,(top-level-aux e0) ,(top-level-aux e1))]
    [`(,e0 ,e1s ...)
     `(,(top-level-aux e0) ,@(map top-level-aux e1s))]))

(define (top-level e)
  `(guard (e [else '"uncaught exception"]) ,(top-level-aux e)))

; I, Aaron Hall, pledge on my honor that I have not given or received any
; unauthorized assistance on this assignment.

