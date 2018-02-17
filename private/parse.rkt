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
      [(hash-ref env sig #f) env]  
      [else 
        (define id (hash-count env))     
        (hash-set env sig id)]))

  (s:iterate/fold store-signal env^ sig))

(define (parse-op op env) 
  (match op
    [(s:Note sig _start _duration _params) (store-signal sig env)]
    [_ env]))

(define (parse-score sco)
  (s:iterate/fold parse-op u:empty-env sco))

