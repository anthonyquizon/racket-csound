#lang racket

(require (for-syntax syntax/to-string))

(define c 0)
(define env (make-hash))
(define (gen-uid)
  (set! c (add1 c))
  c)

(struct Signal (id opcode args))

(define ((signal opcode) . args ) 
  (define id (gen-uid))
  (define s (Signal id opcode args))
  (hash-set! env id s) 
  s)

(define-syntax (define-signal stx)
  (syntax-case stx ()
    [(_ var (args ...))
     (with-syntax ([opcode (syntax->string #'(var))])
       (identifier? #'var)
       (syntax/loc stx (define var (signal opcode))))]))

;(define (+ args)
  ;(cond 
    ;[(map Signal? args) `()]
    ;)
  ;)

;(define (app x ...)
  ;(cond
   ;[(Signal? )`(sig-ref ,i ,x ,y)])
  ;)

;(define foo
 ;'(
  ;(define a (oscil 100 200)) 
  ;(define (b x) (oscil x 300))
  ;(define c (+ a (b 100)))

  ;(a 0 0)
  ;(a 10 20)
  ;((b 200) 0 20)
  ;((b 300) 0 30))

 ;'((sig-ref 1 )
   ;)

 ;)

(define-signal oscil (xamp xcps))
