#lang racket

(define a 
  (instr (oscils a1 1000 550 1) 
         (out a1)))

(a 0 10)
