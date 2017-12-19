#lang racket

(require "compile.rkt")
(require "utils.rkt")

(define args (vector->list (current-command-line-arguments)))
(when (= (length args) 0)
  (display "usage: racket compile-scheme.rkt <.scm file>")
  (exit 1))
(define filename (string->path (car args)))
(define progname (string-trim (car args) ".scm" #:left? #f #:repeat? #f))
(compile-to-binary (with-input-from-file filename read-begin #:mode 'text) progname)