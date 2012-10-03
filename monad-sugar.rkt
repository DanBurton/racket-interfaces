#lang racket

(require
 "interface.rkt")

;; XXX would interface-out be valuable?
;; (provide (interface-out monad))
(provide
 monad
 bind
 return
 do
 do-with-monad)

(define-interface monad
  (bind return))

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

(define-syntax-rule (do-with-monad m . body)
  (with-instance
   monad m
   (do . body)))
