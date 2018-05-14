#lang nanopass

(provide LP L0 parse-LP verify-scheme)
(require (prefix-in m: racket/match))

(define variable? symbol?)
  
(define (primitive? x)
  (define primitives '(+ - * /))
  (and (memq x primitives) #t)) 

(define constant?
  (disjoin null? number? char? boolean? string?)) 

(define datum?
  (lambda (x)
    (or (constant? x)
        (null? x)
        (if (pair? x)
            (and (datum? (car x)) (datum? (cdr x)))
            (and (vector? x) (andmap datum? (vector->list x))))))) 

(define-language LP
  (terminals
    (variable (x))
    (datum (d))
    (primitive (pr))) 
  (Expr (e body)
    d
    x
    pr
    ;;define-signal
    (lambda (x ...) body1 ... body2)
    (e0 e1 ...)))

(define-parser parse-LP LP)

(define-language L0 (extends LP)
  (Expr (e body)
    (- d
       x
       pr
       (e0 e1 ...))
    (+ (datum d)
       (var x)
       (primapp pr e ...)
       (app e0 e1 ...))))

;(define-language L1 (extends L0)
  ;(Expr (e body)
    ;(- (datum d)
       ;(var x)
       ;(app e0 e1 ...))))

(define-pass verify-scheme : LP (ir hello) -> L0 ()
  (definitions
    (define (invalid-var? x env)
      (cond
        [(memq x env) #f]
        [(keyword? x) "keyword"]
        [(primitive? x) "primitive"]
        [else "unbound variable"]))
    (define (valid-bindings? ls) (andmap variable? ls))
    (define (duplicate-names? var*)
      (let f ([ls var*] [dups '()])
        (cond
          [(null? ls) (if (null? dups) #f dups)]
          [(and (memq (car ls) (cdr ls)) (not (memq (car ls) dups)))
           (f (cdr ls) (cons (car ls) dups))]
          [else (f (cdr ls) dups)])))
    (define (format-list ls)
      (case (length ls)
        [(0) ""]
        [(1) (format "~s" (car ls))]
        [(2) (format "~s and ~s" (car ls) (cadr ls))]
        [else (let f ([a (car ls)] [ls (cdr ls)])
                (if (null? ls)
                  (format "and ~s" a)
                  (format "~s, ~a" a (f (car ls) (cdr ls)))))])))
  (Expr : Expr (ir [env '()]) -> Expr ()
    [,d `(datum ,d)]
    [,x (let ([invalid? (invalid-var? x env)])
          (if invalid?
              (error 'verify-scheme "reference to ~a ~s" invalid? x)
              `(var ,x)))]
    [(lambda (,x ...) ,body1 ... ,body2)
     (cond
       [(not (valid-bindings? x))
        (error 'verify-scheme "invalid binding list ~a in lambda form" x)]
       [(duplicate-names? x) =>
        (lambda (x)
          (error 'verify-scheme
            (format "duplicate bindings ~a in lambda form"
              (format-list x))))]
       [else
        (let ([env (append env x)])
          (let ([body1 (map (lambda (x) (Expr x env)) body1)]
                [body2 (Expr body2 env)])
            `(lambda (,x ...) ,body1 ... ,body2)))])]
    [(,e0 ,e1 ...)
     (let ([e1 (map (lambda (x) (Expr x env)) e1)])
       (if (and (symbol? e0) (primitive? e0))
           `(primapp ,e0 ,e1 ...)
           `(app ,(Expr e0 env) ,e1 ...)))])
  (Expr ir hello)
  )

;(define-pass generate-js : L0 (ir) -> L1 ()
  ;(definitions
    ;(define (~number x) (~a x))
    ;(define (~datum d) 
      ;(m:match d
        ;[(? number?) (~number d)]))
    ;(define (~var x) (format "var: ~a" x))
    ;(define (~app id args) (format "app - id: ~a, args: ~a" id args))
    ;(define (~lambda formals body ret) (format "lambda - formals: ~a, body: ~a, ret: ~a" formals body ret)))
  ;(Expr : Expr (ir) -> L1 ()
    ;[(datum ,d) (~datum d)]
    ;[(var ,x) (~var x)]
    ;[(app ,e0 ,e1 ...) (~app e0 e1)]
    ;[(lambda (,x ...) ,body1 ... ,body2) (~lambda x body1 body2)]))

(define-pass generate-js : L0 (ir foo) -> * ()
  (definitions)
  (Expr : Expr (ir) -> * ()
    [(datum ,d) "datum"]
    [(var ,x) "var"]
    [(app ,e0 ,e1 ...) "app"]
    [(primapp ,pr ,e ...) (foo "primapp")]
    [(lambda (,x ...) ,body1 ... ,body2) "lambda"]))

(generate-js 
  (verify-scheme 
    (parse-LP '(+ a 2)) '(a))
  (lambda (x) (displayln (format "hello ~a" x))))

;(parse-LP '(+ a 2))

