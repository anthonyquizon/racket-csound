#lang nanopass

(provide compile)

(require "compiler/languages.rkt"
         "compiler/passes/consolidate-orc-and-sco.rkt"
         "compiler/passes/output-csd.rkt"
         threading)

(module+ test 
  (require rackunit))

(define (compile src)
  (define-parser parse L0)
  (~> src 
      parse
      consolidate-orc-and-sco
      output-csd))

(module+ test
   (test-case
     "compile"
    (check-equal?
      (compile
            '(csd
               (instr 1 
                      (stmt aSin oscil 100 200)
                      (out aSin))
               (i 1 0 0)
               (i 1 200 10)))
      (CSound "instr 1\n aSin oscil 100,200\n out aSin" 
              "i 1 0 0\ni 1 200 10"))))

