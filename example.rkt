#lang racket

(require "./private/constructor.rkt")


(define c
  (oscils b))

(define b
  (oscils c))

(define a 
  (oscils 10000 440))

(define sco
  (score 
    (note a 0 0.5 0)))

