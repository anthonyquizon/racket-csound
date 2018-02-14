#lang racket

(require (prefix-in s: "./struct.rkt")
         (prefix-in u: "./util.rkt")
         racket/format 
         threading)

(provide parse-score)


;;TODO stop circular defines
(define (store-signal sig env)
  (define env^ 
    (cond 
      [(hash-ref env sig) env]  
      [else 
        (define id (hash-count env))     
        (hash-set env sig id)]))

  (u:iterate-signal sig env^ store-signal))

(define (parse-op op env) 
  (display op)
  (match op
    [(s:Note sig _start _duration _params) (store-signal sig env)]
    [_ env]))

(define (parse-score sco)
  (u:iterate-score sco (make-hash '()) parse-op))

