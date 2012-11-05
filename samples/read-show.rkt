#lang racket/base
(require racket/port
         "interface.rkt")

(define-interface showable
  (show))

(define-interface readable
  (tell))

(define-instance showable show-any
  (define (show x)
    (with-output-to-string 
     (λ () (write x)))))

(define-instance readable read-any
  (define (tell str)
    (with-input-from-string
     str read)))

(struct foo (asdf) #:prefab)

#;(define-instance readable read-foo
  (define (my-read str)
    (match (read (open-input-string str)))))

(require rackunit)

(check-equal?
 (with-instances
  ([readable read-any])
  (foo-asdf (tell "#s(foo 3)")))
 3)

(check-equal?
 (with-instances 
  ([readable read-any]
   [showable show-any])
  (foo-asdf (tell (show (foo 3)))))
 3)