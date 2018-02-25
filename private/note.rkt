#lang racket

(require (prefix-in g: "generics.rkt")
         (prefix-in s: "signal.rkt"))

(provide note
         Note)

(define (note-parse note env)
  (define sig (Note-sig note))
  (g:parse sig))

(define (note-render note env) null)

(define (note sig start duration . params)
  (Note sig start duration params))

(struct Note (sig start duration params)
  #:methods g:gen:renderable
  [(define (render note env) (note-render note env))
   (define (parse note env) (note-parse note env))])


