#lang racket/base

(require "interface.rkt")

(define-interface showable
  (show))

(define-interface readable
  (my-read))

(define-instance showable show-any
  (define (show x)
    (format "~s" x)))

(define-instance readable read-any
  (define (my-read str)
    (read (open-input-string str))))

(struct foo (asdf) #:prefab)

#;(define-instance readable read-foo
    (define (my-read str)
      (match (read (open-input-string str))
        )))

#;(with-instances ([readable read-any])
                  (foo-asdf (my-read "#s(foo 3)")))

(with-instances 
 ([readable read-any]
  [showable show-any])
 (foo-asdf (my-read (show (foo 3)))))