#lang racket

(require (prefix-in s: "./struct.rkt"))

(provide sine
         note
         score
         GEN_sin
         header-defaults)

;;TODO audio rate and sample rate

(define header-defaults 
  (make-hash '(("sr" . 44100)
               ("ksmp" . 32)
               ("nchnls" . 2)
               ("0dbfs" . 1))))

(define (sine amp freq)
  (s:Signal 'oscils `(,amp ,freq)))

(define GEN_sin (s:GEN 10))

(define (note sig start duration . params)
  (s:Note sig start duration params))

(define (score . ops) (s:Score ops))

