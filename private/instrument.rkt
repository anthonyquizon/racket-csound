#lang racket

(require (prefix-in g: "generics.rkt")
         (prefix-in s: "signal.rkt"))

(provide Instrument)

(define (instr-parse instr env)
  (g:parse sig))

(define (instr-render instr env) 
  ;;if already in env
  null)

(struct Instrument (sig)
  #:methods g:gen:renderable
  [(define (render instr env) (instr-render instr env))
   (define (parse instr env) (instr-parse instr env))])


