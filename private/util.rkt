#lang racket

(require (prefix-in s: "./struct.rkt")
         racket/format 
         threading)

(provide iterate-signal
         iterate-score)

(define (iterate-signal sig initial fn)
  (define args (s:Signal-args sig))

  (for/fold ([acc initial]) ([arg args])
    (cond
      [(s:Signal? arg) (fn arg acc)]
      [else acc])))

(define (iterate-score sco initial fn)
  (define ops (s:Score-ops sco))

  (for/fold ([acc initial]) ([op ops]) (fn op acc)))
