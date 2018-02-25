#lang racket

(require (prefix-in g: "generics.rkt")
         "env.rkt"
         racket/match
         threading)

(provide Signal
         sine)

(define (render-signal-id sig env type)
  (define id (hash-ref env sig))
  (cond
    [(equal? type 'audio) (format "a~a" id)]
    [(equal? type 'control) (format "k~a" id)]))

(define (render-signal-args sig env)
  (define (f x)
   (cond
    [(Signal? x) (render-signal-id x env 'control)]
    [else (~a x)]))
  
  (~> (Signal-args sig) 
      (map f _)
      (string-join _ ", ")))

(define (signal-iterate/fold fn initial sig)
  (define args (Signal-args sig))

  (for/fold ([acc initial]) ([arg args])
    (cond
      [(Signal? arg) (fn arg acc)]
      [else acc])))

(define (signal-parse sig env) 
  (define (fn env^)
    (g:iterate/fold signal-parse env^ sig))

  (store sig sigs env fn))

(define (signal-render sig env)
  (define sig-env (Env-sigs env))
  (define op (Signal-op sig))
  (define id (render-signal-id sig sig-env 'audio))
  (define args (render-signal-args sig sig-env))

  (format "~a ~a ~a" id op args))

(define (sine amp freq)
  (Signal 'oscils `(,amp ,freq)))

(struct Signal (op args)
  #:methods g:gen:renderable
  [(define (render sig env) (signal-render sig env))
   (define (parse sig env) (signal-parse sig env))]

  #:methods g:gen:iterable
  [(define (iterate/fold fn initial sig) 
     (signal-iterate/fold fn initial sig))])

(module+ test
  (require rackunit)
  (define b (sine 0.2 10))
  (define a (sine 0.5 b))

  (define env (g:parse a empty-env))

  (check-equal?
    (Env-sigs env)
    (make-immutable-hash `((,a . 0) (,b . 1))))

  (check-equal?
    (g:render a env)
    "a0 oscils 0.5, k1")

  (check-equal?
    (g:render b env)
    "a1 oscils 0.2, 10"))


