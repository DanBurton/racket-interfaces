#lang racket/base
(require racket/stxparam
         (for-syntax racket/base
                     racket/syntax))

(begin-for-syntax
  (struct interface-info (dyn param static)))

(define-syntax (define-interface stx)
  (syntax-case stx ()
    [(_ id s (f ...))
     (with-syntax
         ([(s-f ...)
           (for/list ([fid (in-list (syntax->list #'(f ...)))])
             (format-id #'s "~a-~a" #'s fid))])
       (syntax/loc stx
         (begin
           (define dynamic-id-param (make-parameter #f))
           (define-syntax (dynamic-id stx)
             #'(dynamic-id-param))
           
           (define-syntax-parameter static-id
             (make-rename-transformer #'dynamic-id))
           (define-syntax id 
             (interface-info #'dynamic-id #'dynamic-id-param #'static-id))
           
           (struct s (f ...))
           
           (define-syntax-rule (f . e)
             ((s-f static-id) . e))
           ...)))]))

(define-syntax (with-generics stx)
  (syntax-case stx ()
    [(_ interface instance . body)
     (quasisyntax/loc stx
       (let ([instance-id instance])
         (syntax-parameterize 
          ([#,(interface-info-static (syntax-local-value #'interface))
            (make-rename-transformer #'instance-id)])
          (parameterize ([#,(interface-info-param (syntax-local-value #'interface))
                          instance-id])
            . body))))]))

(define-syntax (generalized stx)
  (syntax-case stx ()
    [(_ interface . body)
     (quasisyntax/loc stx
       (syntax-parameterize 
        ([#,(interface-info-static (syntax-local-value #'interface)) 
          (make-rename-transformer 
           #'#,(interface-info-dyn (syntax-local-value #'interface)))])
        . body))]))

;; XXX provide for syntax?
(provide
 define-interface
 with-generics
 generalized)

;; XXX move this example to monad.rkt
(require racket/list)

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

