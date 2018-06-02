#lang csound

(define a (oscil 1000 550 1)) 
(define b 10)
(define (c x) (oscils (x b) b c))

(a 0 10)
(b 0 10)
((c ) 0 10)
