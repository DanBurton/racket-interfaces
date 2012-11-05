#lang racket

(require
 "interface.rkt"
 "monad-sugar.rkt")


;; either-t
;; laws:
;; (r? (return ANY)) = true
;; (r? (bind R ANY)) = true
;; and m obeys monad laws

(define (either-t m r?)
  (make-instance monad
    (define (return x)
      (with-instance monad m (return x)))
    (define (bind x f)
      (cond
        ;; XXX problem seems to be here,
        ;; Racket thinks I mean recursive "bind"
        ;; when I really mean "bind" from monad "m"
        [(r? x) (with-instance monad m (bind x f))]
        [else x]))))


;; io
(struct io-action-pure (val))
(struct io-action-impure (thunk))
(define (io-action? x)
  (or (io-action-pure? x) (io-action-impure? x)))

(struct io-error (function message))

(define io-monad
  (make-instance monad
    (define return io-action-pure)
    (define (bind x f)
      (match x
        [(io-action-pure val)
         (f val)]
        [(io-action-impure thunk)
         (io-action-impure
          (λ ()
            (f (thunk))))]))))


;; a few dumb io operations
(define (safe-read-file file)
  (io-action-impure
   (λ ()
     (cond
       [(file-exists? file) (file->string file)]
       [else (io-error "safe-read-file" "File does not exist.")]))))

(define (put-str-ln s)
  (io-action-impure
   (λ ()
     (cond
       [(string? s)
        (write-string s)
        (newline)]
       [else (io-error "put-str-ln" "Input must be a string.")]))))


;; define some sample "main methods"
(define either-io-monad
  (either-t io-monad io-action?))


;; XXX figure out why this spins for so long
#;(define safe-main
  (do-with-monad either-io-monad
    (s <- (safe-read-file "foo.txt"))
    (put-str-ln s)))

;; XXX also why does it say "s" is unbound?
#;(define safe-main-partially-desugared
  (with-instance
   monad either-io-monad
   (do
     (s <- (safe-read-file "foo.txt"))
     (put-str-ln s))))

#;(define safe-main-desugared
  (with-instance
   monad either-io-monad
   (bind (safe-read-file "foo.txt") put-str-ln)))

(define safe-main-simple
  (with-instance
   monad either-io-monad
   (put-str-ln "Hello, world!")))

(define safe-main-err
  (with-instance
   monad either-io-monad
   (put-str-ln (void))))

(define safe-main-file
  (with-instance
   monad (either-t io-monad io-action?) ;; inline expression!
   (safe-read-file "foo.txt")))


;; a way to run these "safe" main methods
(define (run-io io)
  (match io
    [(io-action-pure val) val]
    [(io-action-impure thunk) (thunk)]))

(define (run-safe-main e-io)
  (define result (run-io e-io))
  (cond
    [(io-error? result)
     (write-string
      (string-append
       "error: "
       (io-error-function result)
       ": "
       (io-error-message result)))
     (newline)]
    [(void? result)
     (write-string "Success!")
     (newline)]
    [else
     (error
      'run-safe-main
      (format "unexpected result:~n~a" result))]))

;; testing

#;(run-safe-main safe-main)
#;(run-safe-main safe-main-partially-desugared)
#;(run-safe-main safe-main-desugared)

(run-safe-main safe-main-simple)
(run-safe-main safe-main-err)
(run-safe-main safe-main-file)
