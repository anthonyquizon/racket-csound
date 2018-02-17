#lang racket

(require racket/generic)

(provide (all-defined-out))


;;TODO use collections
(define-generics 
  iterable 
  [iterate/fold fn initial iterable])

(struct Signal (op args)
  #:methods gen:iterable
  [(define (iterate/fold fn initial sig)
     (define args (Signal-args sig))

     (for/fold ([acc initial]) ([arg args])
       (cond
         [(Signal? arg) (fn arg acc)]
         [else acc])))])

(struct Score (ops)
  #:methods gen:iterable
  [(define (iterate/fold fn initial sco)
     (define ops (Score-ops sco))

     (for/fold ([acc initial]) ([op ops]) (fn op acc)))])

(struct GEN (id))
(struct Note (sig start duration params))

