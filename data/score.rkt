#lang racket

(require (prefix-in g: "generics.rkt")
         (prefix-in u: "util.rkt"))

(provide score
         Score)

(define header-defaults 
  (make-hash '(("sr" . 44100)
               ("ksmp" . 32)
               ("nchnls" . 2)
               ("0dbfs" . 1))))

(define (score-parse sco env)
  (g:iterate/fold g:parse u:empty-env sco))

(define (score-render sco env) 
  ;;render instrs
  ;;render notes
  null)

(define (score . ops) (Score ops))

(struct Score (ops)
  #:methods g:gen:renderable
  [(define (parse sco env) (score-parse sco env))
   (define (render sco env) (score-render sco env))]

  #:methods g:gen:iterable
  [(define (iterate/fold fn initial sco)
     (define ops (Score-ops sco))

     (for/fold ([acc initial]) ([op ops]) (fn op acc)))])

