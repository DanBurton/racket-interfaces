#lang racket

(define-syntax-rule (define-interface name (supers ...) body ...)
  (define name (interface (supers ...) body ...)))

(define-interface functor ()
  [fmap f this])
(define-interface monad (functor)
  [return a]
  [bind this f]
  [join this])

(define (monad-instance
         #:return the-return
         #:functor [my-functor-instance #f]
         #:join [my-join #f]
         #:bind [my-bind #f])
  (define my-fmap
    (and my-functor-instance
         (with-generics functor my-functor-instance fmap)))
  (unless (and my-return (or (and my-fmap my-join) my-bind))
    (error 'monad-instance "Must provide functor&join or bind"))
  (define the-bind
    (or my-bind (λ (m f) (my-join (my-fmap f m)))))
  (define the-join
    (or my-join (λ (mm) (my-bind mm identity))))
  (define the-functor-instance
    (or functor-instance (monad->functor the-join the-bind)))
  (instance monad (my-functor-instance)
    ([bind the-bind]
     [return the-return]
     [join the-join])))

(define (monad->functor my-join my-bind)
  (instance functor ()
    [(fmap f x) (my-join (my-bind x f))]))

(struct none ())
(struct some (val))

(define option-functor
  (instance functor ()
    ([(fmap f opt)
      (match opt
        [(none) (none)]
        [(some x) (some (f x))])])))

(define option-monad
  (monad-instance
   #:fmap (with-generics functor option-functor fmap)
   #:return some
   #:join (match-lambda
            [(some (some x)) (some x)]
            [_ (none)])))

(define-syntax do
  (syntax-rules (define/bind <-)
    [(do) pass]
    [(do (define/bind x blah) y ...)
     (bind blah (λ (x) (do y ...)))]
    [(do (x <- blah) y ...)
     (bind blah (λ (x) (do y ...)))]
    [(do x y ...)
     (bind x (const (do y ...)))]))

(define-syntax-rule (do-with m action ...)
  (with-generics monad m
    (do action ...)))

(do-with option-monad
  (foo <- (some 3))
  (none)
  (bar <- (some 4))
  (return (+ foo bar)))

(define forever
  (generalized monad m
    (define (go y) (do y (go y)))
    go))
