#lang racket

(provide ++)

(define (++ . xs) (string-join xs "\n"))
