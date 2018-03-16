#lang racket

(require (prefix-in g: "generics.rkt")
         (prefix-in s: "signal.rkt")
         "env.rkt"
         "util.rkt"
         threading
         )

(provide Instr)

(define (instr-parse instr env) 
  (define sig (Instr-sig instr))
  (define env^ (g:parse sig env))

  (store instr instrs env^ identity))

(define (instr-render instr env) 
  (define id 
    (~> env Env-instrs (hash-ref _ instr)))

  (define out 
    (~> instr Instr-sig (g:render _ env)))

  (format "instr ~a\n~a\nendin\n" id out))

(struct Instr (sig)
  #:methods g:gen:renderable
  [(define (render instr env) (instr-render instr env))
   (define (parse instr env) (instr-parse instr env))])

(module+ test
  (require rackunit)

  (define sig (s:sine 0.5 100))
  (define instr (Instr sig))
  (define env (g:parse instr empty-env))

  (check-equal?
    (Env-instrs env)
    (make-immutable-hash `((,instr . 0))))

  (check-equal?
    (Env-sigs env)
    (make-immutable-hash `((,sig . 0))))

  (check-equal?
    (g:render instr env)
    (++ "instr 0"
        "a0 oscils 0.5, 100"
        "out a0"
        "endin")))
