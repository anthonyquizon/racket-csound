#lang racket

(require "./private/signal.rkt"
         "./private/score.rkt"
         "./private/note.rkt")


(define a 
  (sine 0.5 440))

(define sco
  (score 
    (note a 0 0.5 0)))

