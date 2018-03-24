#lang typed/racket

(require/typed (prefix-in s: "../data/signal.rkt")
               [#:struct s:Signal ([id : Real] [y : Real])]
               )

(provide sine)

(define (sine amp freq)
  ;;if needs control signal
  (s:Signal 'oscils `(,amp ,freq)))
