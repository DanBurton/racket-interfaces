#lang racket

;; wrapped-functor : functor?
;; base-monad : monad?
;; r? : any -> bool | r
(define (free-t r? wrapped-functor base-monad)
  (instance
   monad
   ;; return : R -> FreeT F M R
   (define (return r) r)
   (define (bind m f)
     (cond
       [(r? m) (f m)]
       [else ((fmap-with wrapped-functor)
              (curry (bind-with base-monad) f)
              m)]))))

