#lang racket/base
(require racket/stxparam
         (for-syntax syntax/parse
                     racket/base
                     racket/function
                     syntax/strip-context
                     racket/syntax))

(begin-for-syntax
  (struct interface-info (dyn param static dict members)))

(define-syntax (define-interface stx)
  (syntax-parse stx
    [(_ interface:id (member:id ...))
     (with-syntax*
      ([dict (generate-temporary #'interface)]
       [(dict-member ...)
        (for/list ([mid (in-list (syntax->list #'(member ...)))])
          (format-id #'dict "~a-~a" #'dict mid))])
      (syntax/loc stx
        (begin
          (define dynamic-id-param (make-parameter #f))
          (define-syntax (dynamic-id stx)
            #'(dynamic-id-param))

          (define-syntax-parameter static-id
            (make-rename-transformer #'dynamic-id))
          (define-syntax interface
            (interface-info #'dynamic-id #'dynamic-id-param #'static-id
                            #'dict
                            #'(member ...)))

          (struct dict (member ...))

          (define-syntax-rule (member . e)
            ((dict-member static-id) . e))
          ...)))]))

(define-syntax (with-generics stx)
  (syntax-parse stx
    [(_ interface instance:id . body:expr)
     #:declare interface (static interface-info? "interface")
     (quasisyntax/loc stx
       (let ([instance-id instance])
         (syntax-parameterize
             ([#,(interface-info-static (attribute interface.value))
               (make-rename-transformer #'instance-id)])
           (parameterize
               ([#,(interface-info-param (attribute interface.value))
                 instance-id])
             . body))))]))

(define-syntax (generalized stx)
  (syntax-parse stx
    [(_ interface . body:expr)
     #:declare interface (static interface-info? "interface")
     (quasisyntax/loc stx
       (syntax-parameterize
           ([#,(interface-info-static (attribute interface.value))
             (make-rename-transformer
              #'#,(interface-info-dyn (attribute interface.value)))])
         . body))]))

(define-syntax (define-instance stx)
  (syntax-parse stx
    [(_ interface instance:id body:expr ...)
     #:declare interface (static interface-info? "interface")
     (with-syntax
         ([dict
           (interface-info-dict (attribute interface.value))]
          [(member ...)
           (map
            (curry replace-context #'instance)
            (syntax->list
             (interface-info-members (attribute interface.value))))])
       (syntax/loc stx
         (define instance
           (let ()
             body ...
             (dict member ...)))))]))

(provide
 define-interface
 define-instance
 with-generics
 generalized)
