#lang racket/base
(require racket/list
         "interface.rkt")

(define-interface monad
  (bind return))

(define-instance monad list-monad
  (define (bind m f)
    (append-map f m))
  (define (return a)
    (list a)))

(define-instance monad maybe-monad
  (define (bind m f)
    (and m (f m)))
  (define (return a)
    a))

(with-generics
 monad maybe-monad
 (define (just x) 
   (return x))
 (define (fmap f xs)
   (generalized monad
                (bind xs
                      (λ (x) (return (f x))))))
 (print (fmap add1 4))
 (define (list-add-one xs)
   (with-generics
    monad list-monad
    (fmap add1 (just xs))))
 (list-add-one '(1 2 3)))

