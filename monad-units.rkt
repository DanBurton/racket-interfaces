#lang racket

(define-signature
  monad^
  (return bind))
(define-signature
  monad-zero^
  (mzero))
(define-signature
  monad-fail^
  (mfail))

(define-unit
  list@
  (import)
  (export monad^ monad-zero^ monad-fail^)

  (define (return a) (list a))
  (define (bind m f)
    (append-map f m))

  (define (mzero) empty)
  (define (mfail str) empty))

(define-unit
  maybe@
  (import)
  (export monad^ monad-zero^ monad-fail^)

  (define (return a) a)
  (define (bind m f) (and m (f m)))

  (define (mzero) #f)
  (define (mfail str) #f))

(define-unit
  cont@
  (import)
  (export monad^ monad-fail^)
  
  (define (return a) (λ (k) (k a)))
  (define (bind m k) (λ (c) (m (λ (a) ((k a) c)))))
  
  (define (mfail str) (error 'fail/cont "failed")))


(define-values/invoke-unit/infer
  maybe@)

(define pass (return (void)))
(define (lift-io x) pass)

(require (for-syntax syntax/parse))
(define-syntax (<- stx)
  (raise-syntax-error '<- "Not valid outside mcomp or do" stx))
(define-syntax (<~ stx)
  (raise-syntax-error '<~ "Not valid outside mcomp or do" stx))

(define-syntax (do stx)
  (syntax-parse
   stx #:literals (<- <~ define match-define)
   [(_ (define x:id val:expr) rest ...)
    (syntax/loc stx
      ((λ (x) (do rest ...)) val))]
   [(_ (match-define pat:expr val:expr) rest ...)
    (syntax/loc stx
      ((match-lambda
         [pat (do rest ...)]
         [_ (mfail "Failed match.")]) val))] ;; XXX sprintf this
   [(_ (~and
        (_ <- _)
        ~!
        (x:id <- val:expr)) rest ...)
    (syntax/loc stx
      (bind val (λ (x) (do rest ...))))]
   [(_ (~and
        (_ <~ _)
        ~!
        (pat:expr <~ val:expr)) rest ...)
    (syntax/loc stx
      (bind val (match-lambda
                  [pat (do rest ...)]
                  [_ (mfail "Failed match.")])))] ;; XXX sprintf this
   [(_ x:expr)
    (syntax/loc stx
      x)]
   [(_ x:expr rest ...)
    (syntax/loc stx
      (bind x (λ (_) (do rest ...))))]
   [(_)
    (syntax/loc stx
      pass)]))

(define (guard x)
  (if x pass (mzero)))


(define-syntax (mcomp stx)
  (syntax-parse 
   stx #:literals (<- <~)
   [(_ r:expr)
    (syntax/loc stx
      (return r))]
   [(_ (~and 
        (_ <- _)
        ~!
        (x:id <- y:expr)) clause ...)
    (syntax/loc stx
      (bind y (lambda (x) (mcomp clause ...))))]
   [(_ (x:expr <~ y:expr) clause ...)
    (syntax/loc stx
      (bind y (match-lambda [x (mcomp clause ...)]
                            [_ (mzero)])))]
   [(_ b:expr clause ...)
    (syntax/loc stx
      (bind (guard b)
            (λ (_) (mcomp clause ...))))]))



;; do

(require rackunit)

(check-equal?
 (do
   (x <- (return 3))
   (y <- (return 4))
   (return (+ x y)))
 7)


(check-equal?
 (do (x <- (return 3)))
 (void))

;; forever

(define (forever x)
  (do x (forever x)))

(check-equal?
 (forever
  (do
    (x <- (return 3))
    (y <- (return 4))
    ;(lift-io (print x))
    (guard (= x y))))
 #f)

;; monad comprehensions

(check-equal?
  (mcomp ((cons x 1) <~ (cons 2 1))
         (y <- 2)
         (< 0 (+ x y))
         (cons x y))
  '(2 . 2))
