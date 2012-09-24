#lang racket/base
(require racket/stxparam
         (for-syntax racket/base
                     racket/function
                     syntax/strip-context
                     racket/syntax))

(begin-for-syntax
  (struct interface-info (dyn param static dict fields)))

(define-syntax (define-interface stx)
  (syntax-case stx ()
    [(_ id (f ...))
     (with-syntax*
      ([s (generate-temporary #'id)]
       [(s-f ...)
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
            (interface-info #'dynamic-id #'dynamic-id-param #'static-id
                            #'s
                            #'(f ...)))

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
           (parameterize
               ([#,(interface-info-param (syntax-local-value #'interface))
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

(define-syntax (define-instance stx)
  (syntax-case stx ()
    [(_ interface instance body ...)
     (with-syntax
         ([s
           (interface-info-dict (syntax-local-value #'interface))]
          [(f ...)
           (map
            (curry replace-context #'instance)
            (syntax->list
             (interface-info-fields (syntax-local-value #'interface))))])
       (syntax/loc stx
         (define instance
           (let ()
             body ...
             (s f ...)))))]))

(provide
 define-interface
 define-instance
 with-generics
 generalized)
