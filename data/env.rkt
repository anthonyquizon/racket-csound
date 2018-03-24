#lang typed/racket

(require racket/format 
         threading
         (for-syntax racket/syntax))

(provide empty-env
         Env
         store)

(define-type Env (HashTable Symbol Any))
(define empty-env (hash))

(define-syntax (store stx)
  (syntax-case stx ()
    [(_ val key env id-fn fn)
     (with-syntax  
       ([getter (format-id stx "Env-~a" #'key)])
       #`(let* ([val-env (getter env)]
                [val-env^ 
                  (cond
                    [(hash-ref val-env val #f) val-env]
                    [else
                      (define id (hash-count val-env))
                      (hash-set val-env val (id-fn id))])]
                [env^ (struct-copy Env env [key val-env^])])
           (fn env^)))]))

(: store (-> Env Symbol) )
(define (store env key val key-fn fn)
  (define env^ 
    (cond 
      [(hash-ref env key #f) ]
      ))
  )
