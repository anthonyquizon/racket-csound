#lang typed/racket

(require (prefix-in g: "generics.rkt")
         "env.rkt"
         "../base/util.rkt"
         racket/match
         threading)

(provide Signal)

(struct Signal-Store 
  ([id : Integer]
   [rendered : String] 
   [children : (Listof Signal-Store)]) #:transparent) 

(define-type Signal-Control)
(define-type Signal-Audio)
(define-type Signal-Arg (U Signal Real))

(struct Signal ([op : Symbol] [args : (Listof Signal-Arg)])
  #:methods g:gen:renderable
  [(define (render sig env) (signal-render sig env))
   (define (parse sig env) (signal-parse sig env))]

  #:methods g:gen:iterable
  [(define (iterate/fold fn initial sig) 
     (signal-iterate/fold fn initial sig))])


;;TODO replace these with state monad?
(: )
(define (get-id env sig) 
  (~> (hash-ref env sig) Signal-Store-id))

(define (get-rendered env sig) 
  (~> (hash-ref env sig) Signal-Store-rendered))

(define (get-type env sig) 
  (~> (hash-ref env sig) Signal-Store-type))

(define (get-children env sig) 
  (~> (hash-ref env sig) Signal-Store-children))

(define (id->string env sig)
  (define id (get-id env sig)) ;;TODO replace these with state monad?
  (define type (get-type env sig))
  (cond
    [(equal? type 'audio) (format "a~a" id)]
    [else (format "k~a" id)]))

(define (signal-iterate/fold fn initial sig)
  (define args (Signal-args sig))

  (for/fold ([acc initial]) ([arg args])
    (cond
      [(Signal? arg) (fn arg acc)]
      [else acc])))

(define (signal-parse sig env) 
  (define (fn env^) (g:iterate/fold signal-parse env^ sig))
  (define (id-fn id) (Signal-Store id null null null))
  (store sig sigs env id-fn fn))

(define ((arg->string env) arg)
  (cond
    [(Signal? arg) (id->string env arg)]
    [else (~a arg)]))

(define (args->string env sig)
  (~> (Signal-args sig) 
      (map (arg->string env) _)
      (string-join _ ", ")))

(define ((update-store rendered children) sig-store)
  (struct-copy Signal-Store sig-store
               [rendered rendered]
               [children children]))

(define (sub-signals sig)
  (~>> (Signal-args sig)
       (filter Signal?)))

(define (child-signals->rendered-env env children)
  (for/fold 
    ([env^ env]) 
    ([sig children])
    (signal->rendered-env env^ sig)))

(define (signal->rendered-env env sig)
  (define str-op (Signal-op sig))
  (define str-id (id->string env sig))
  (define str-args (args->string env sig))
  (define str (format "~a ~a ~a" str-id str-op str-args)) 
  (define children (sub-signals sig))
  (define env^ (child-signals->rendered-env env children)) ;;TODO use iterate/fold?

  (hash-update env^ sig (update-store str children)))

(define (rendered-env->string env sig)
  (define id (id->string env sig))
  (define rendered (get-rendered env sig))
  (format "~a\nout ~a" rendered id))

(define (signal-render sig env)
  (~> (Env-sigs env)  
      (signal->rendered-env _ sig)
      (rendered-env->string _ sig)))

(module+ test
  (require rackunit)

  (define b (Signal 'oscils 0.2 10))
  (define a (Signal 'oscils 0.5 b))
  (define c (Signal 'oscils 0.3 b))

  (define env (g:parse a empty-env))

  (check-equal?
    (Env-sigs env)
    (hash a (Signal-Store 0 null null null)  
          b (Signal-Store 1 null null null)))

  (check-equal?
    (g:render a env)
    (++ "k1 oscils 0.2, 10"
        "a0 oscils 0.5, k1"
        "out a0"))

  (check-equal?
    (g:render b env)
    (++ "a1 oscils 0.2, 10"
        "out a1")))

