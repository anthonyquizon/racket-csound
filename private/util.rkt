#lang racket

(require racket/format 
         threading)

(provide empty-env)

(define empty-env (make-immutable-hash '()))

