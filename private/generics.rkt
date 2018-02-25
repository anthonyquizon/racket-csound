#lang racket

(require racket/generic)

(provide (all-defined-out))

(define-generics 
  iterable 
  [iterate/fold fn initial iterable])

(define-generics 
  renderable 
  [parse renderable env]
  [render renderable env])

