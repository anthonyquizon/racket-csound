#lang racket

(require (prefix-in c: "./constructor.rkt")
         (prefix-in s: "./struct.rkt")
         (prefix-in u: "./util.rkt")
         (prefix-in p: "./parse.rkt")
         racket/format 
         threading)

(module+ test
  (require rackunit))

(define (render-audio-var id)
  (format "a~a" id))

(define (render-control-var id)
  (format "k~a" id))

(define (render-signal-id sig env)
  ;;TODO check if control or audio
  (~> (hash-ref env sig) render-audio-var))

(define (render-signal-args sig env)
  ;;TODO audio ids
  (~> (s:Signal-args sig) 
      (map ~a)
      (string-join ", ")))

(define ((render-signal env) sig acc)
  (define op (s:Signal-op sig))
  (define id (render-signal-id sig env))
  (define args (render-signal-args sig env))
  
  (format "~a ~a ~a" op id args))

(define (render-instr sig env)
  (define id (hash-ref env sig))
  (define out (render-signal-id sig env))

  (define head (format "instr ~a \n" id))
  (define body (u:iterate-signal sig "" (render-signal env)))
  (define end (format "out ~a \n endin \n" out))

  (format "~a ~a ~a" head body end))

(define ((op->render-instr env) op acc)
  (match op
    [(s:Note sig _start _duration _params) (render-instr sig env)]
    [_ acc]))

(define (render-assign k v)
  (format "~a = ~a" k v))

(define (render-header header)
  (~> (hash-map header render-assign)
      (string-join "\n")))

(define (render-orc sco env)
  (define instrs (u:iterate-score sco "" (op->render-instr env)))
  (define header (render-header c:header-defaults))
  (format "~a \n ~a" header instrs)) 

(define (render-score sco env) "")

(define (render sco)
  (define env (p:parse-score sco))

  (values 
    (render-orc sco env) 
    (render-score sco env)))

(module+ test
  (define a (c:sine 1000 440))
  (define sco (c:score (c:note a 0 0.5 0)))

  (render sco)

  )
