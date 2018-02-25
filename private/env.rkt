#lang racket

(require racket/format 
         threading
         (for-syntax racket/syntax))

(provide empty-env
         Env-sigs
         Env-instrs
         Env
         Env?
         store)

(struct Env (instrs sigs))

(define empty-env (Env (make-immutable-hash '())
                       (make-immutable-hash '()))) 

(define-syntax (store stx)
  (syntax-case stx ()
    [(_ val key env fn)
     (with-syntax  
       ([getter (format-id stx "Env-~a" #'key)])
       #`(let* ([val-env (getter env)]
                [val-env^ 
                  (cond
                    [(hash-ref val-env val #f) val-env]
                    [else
                      (define id (hash-count val-env))
                      (hash-set val-env val id)])]
                [env^ (struct-copy Env env [key val-env^])])
           (fn env^)))]))

