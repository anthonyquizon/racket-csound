#lang racket

(require (prefix-in s: "./struct.rkt")
         racket/format 
         threading)

(provide empty-env)

(define empty-env (make-immutable-hash '()))

