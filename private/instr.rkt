#lang racket

(require (prefix-in g: "generics.rkt")
         (prefix-in e: "env.rkt")
         (prefix-in s: "signal.rkt"))

(provide Instrument)

(define (instr-parse instr env)
  (define instr-env (e:Env-instr env))
  (define instr-env^
    (cond 
      [(hash-ref instr-env instr #f) instr-env]  
      [else 
        (define id (hash-count env))     
        (define env^ (hash-set env sig id))
        (g:iterate/fold signal-parse env^ sig)]))
  (define env^ (g:parse sig env))

  (struct-copy e:Env env^ [instr instr-env])
  )

(define (instr-render instr env) 
  ;;if already in env
  null)

(struct Instrument (sig)
  #:methods g:gen:renderable
  [(define (render instr env) (instr-render instr env))
   (define (parse instr env) (instr-parse instr env))])


