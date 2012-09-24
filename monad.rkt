#lang racket/base
(require racket/list
         "interface.rkt")

(define-interface monad
  monad-dict
  (bind return))

(define list-monad
  (monad-dict
   (λ (m f) (append-map f m))
   (λ (a) (list a))))

(define maybe-monad
  (monad-dict
   (λ (m f) (and m (f m)))
   (λ (x) x)))

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

