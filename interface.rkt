#lang racket/base
(require racket/stxparam
         racket/splicing
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

          (define-syntax member
            (make-set!-transformer
             (lambda (stx)
               (syntax-case stx (set!)
                 [(set! id v)
                  (raise-syntax-error
                   'member
                   "Cannot rebind interface members"
                   stx)]
                 [(id . e)
                  (syntax/loc stx
                    ((dict-member static-id) . e))]
                 [id (identifier? #'id)
                     (syntax/loc stx
                       (dict-member static-id))]))))
          ...)))]))

(define-syntax (with-instance stx)
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

(define-syntax (with-interface stx)
  (syntax-parse stx
    [(_ interface . body:expr)
     #:declare interface (static interface-info? "interface")
     (quasisyntax/loc stx
       (syntax-parameterize
           ([#,(interface-info-static (attribute interface.value))
             (make-rename-transformer
              #'#,(interface-info-dyn (attribute interface.value)))])
         . body))]))

(define-syntax (make-instance stx)
  (syntax-parse stx
    [(_ interface . body:expr)
     #:declare interface (static interface-info? "interface")
     (with-syntax
         ([dict
           (interface-info-dict (attribute interface.value))]
          [(member ...)
           (map
            (curry replace-context #'body)
            (syntax->list
             (interface-info-members (attribute interface.value))))])
       (syntax/loc stx
         (let ()
           (splicing-let () . body)
           (dict member ...))))]))

(define-syntax (define-instance stx)
  (syntax-parse stx
    [(_ interface instance:id . body:expr)
     (syntax/loc stx
       (define instance (make-instance interface . body)))]))

(provide
 define-interface
 make-instance
 define-instance
 with-instance
 with-interface)
