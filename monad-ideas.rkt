#lang racket

(require
 "interface.rkt"
 "monad-sugar.rkt")

(define-interface functor
  (fmap))

(define (monad-instance
         #:return the-return
         #:functor [my-functor-instance #f]
         #:join [my-join #f]
         #:bind [my-bind #f])
  (define my-fmap
    (and my-functor-instance
         (with-instance functor my-functor-instance fmap)))
  (unless (and the-return (or (and my-fmap my-join) my-bind))
    (error 'monad-instance "Must provide functor&join or bind"))
  (define the-bind
    (or my-bind (λ (m f) (my-join (my-fmap f m)))))
  (define the-join
    (or my-join (λ (mm) (my-bind mm identity))))
  (define the-functor-instance
    (or my-functor-instance (monad->functor the-join the-bind)))
  (make-instance monad
    (define bind the-bind)
    (define return the-return)
    #;(define join the-join)
    ))

(define (monad->functor my-join my-bind)
  (make-instance functor #;()
    #;[(fmap f x) (my-join (my-bind x f))]
    (define (fmap f x) (my-join (my-bind x f)))))

(struct none ())
(struct some (val) #:transparent)

(define option-functor
  (make-instance
   functor
   (define (fmap f opt)
     (match opt
       [(none) (none)]
       [(some x) (some (f x))]))))

(define option-monad
  (monad-instance
   #:functor option-functor
   #:return some
   #:join (match-lambda
            [(some (some x)) (some x)]
            [_ (none)])))


(define-syntax-rule (do-with m action ...)
  (with-instance monad m
    (do action ...)))


#;(with-instance
 monad option-monad
 (do
   (foo <- (some 3))
   ;(none)
   (bar <- (some 4))
   (return (+ foo bar))))

(define forever
  (with-interface
   monad
   (define (go y) (do y (go y)))
   go))
