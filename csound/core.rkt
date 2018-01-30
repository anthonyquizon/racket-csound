#lang racket

(require "ffi.rkt")
(provide run)

(define (run orc sco)
  (define cs (csound-create))

  (csound-initialize 3)
  (csound-set-option cs "-odac") 
  (csound-compile-orc cs orc)
  (csound-read-score cs sco)      
  (csound-start cs)             
  (csound-perform cs)          
  (csound-stop cs)
  (csound-destroy cs))

