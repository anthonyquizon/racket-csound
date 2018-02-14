#lang racket

(require (prefix-in f: "private/ffi.rkt"))

(provide run)

(define (run orc sco)
  ;;TODO render
  (define cs (f:csound-create))

  (f:csound-initialize 3)
  (f:csound-set-option cs "-odac") 
  (f:csound-compile-orc cs orc)
  (f:csound-read-score cs sco)      
  (f:csound-start cs)             
  (f:csound-perform cs)          
  (f:csound-stop cs)
  (f:csound-destroy cs))

