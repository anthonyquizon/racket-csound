#lang racket

(require (prefix-in c: "./constructor.rkt")
         (prefix-in s: "./struct.rkt")
         (prefix-in u: "./util.rkt")
         (prefix-in p: "./parse.rkt")
         racket/format 
         threading)

(module+ test
  (require rackunit))


(define (render-instr sig env)
  (define id (hash-ref env sig))
  (define sig-str ((render-signal env) sig ""))
  (define out (render-signal-id sig env))

  (define head (format "instr ~a \n" id))
  (define body (s:iterate/fold (render-signal env) sig-str sig))
  (define end (format "out ~a \n endin \n" out))

  (format "~a ~a ~a" head body end))

(define ((op->render-instr env) op acc)
  (match op
    [(s:Note sig _start _duration _params) (render-instr sig env)]
    [_ acc]))

(define ((op->render-score-op env) op acc)
  (match op
    [(s:Note sig _start _duration _params) (render-instr sig env)]
    [_ acc]))

(define (render-assign k v)
  (format "~a = ~a" k v))

(define (render-header header)
  (~> (hash-map header render-assign)
      (string-join "\n")))

(define (render-orchestra sco env)
  (define instrs (s:iterate/fold (op->render-instr env) "" sco))
  (define header (render-header c:header-defaults))
  (format "~a \n ~a" header instrs)) 

(define (render-score sco env) 
  (define ops (s:iterate/fold (op->render-score-op env) "" sco))
  (format "~a \n e" ops))

(define (render sco)
  (define env (p:parse-score sco))

  (values (render-orchestra sco env) 
          (render-score sco env)))

(module+ test
  (define sig (c:sine 1000 440))
  (define sco (c:score (c:note sig 0 0.5 0)))

  (define-values (orc-str sco-str) (render sco))

  (display orc-str)

  )
