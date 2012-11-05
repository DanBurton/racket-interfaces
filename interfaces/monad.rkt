#lang racket

(require
 "../interface.rkt")

(provide
 monad
 bind
 return
 pass)

(define-interface monad
  (bind return))

(define (pass)
  (with-interface
   monad
   (return (void))))
