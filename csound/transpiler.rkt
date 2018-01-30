#lang racket

(require racket/format 
         threading)

(define header-defaults 
  '(("sr" . 44100)
    ("ksmp" . 32)
    ("nchnls" . 2)
    ("0dbfs" . 1)))


;;TODO define-signal

(define (oscil a b c)
  (format "oscil ~a, ~a, ~a" a b c))

(define (vco2 a b)
  (format "vco2 ~a, ~a" a b))

;;TODO set as control or audio signal
;;TODO define-signal -> define
(define-syntax (define-signal stx)
  (syntax-case stx ()
    [(_ result action args ...) 
       #'(format "a~a ~a" 'result (action args ...))]))

;; TODO assign names to incremental ints

(define (instr id)
  (define start (format "instr ~a" id))
  (define end "endin")

  (format "~a ~a" start end))

(define (assignment k v)
  (format "~a = ~a" k v))

(define header 
  (map (lambda (x) 
         (match x [(cons k v) (assignment k v)])) 
       header-defaults))

(define render
  (displayln "TODO"))
