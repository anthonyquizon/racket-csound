#lang nanopass

(define primitives '(oscil))

(define (Primitive? v) (memq v primitives))
(define (Id? v) (number? v))
(define (Out? v) (equal? v 'out))
(define (Argument? v) (number? v))
(define (Variable? v) ;;TODO control or audio rate 
  (and (symbol? v) (not (Primitive? v)))) 

(define-language LINST 
  (entry Instr)
  (terminals 
    (Argument (a))
    (Id (i))
    (Variable (v))
    (Out (o))
    (Primitive (p)))
  (Stmt (S)
    (stmt v p a1 ...))
  (Instr (I)
    (instr i S1 ... (o v)))
  )

(define-pass pass-instr : LINST (ir) -> * ()
  (definitions 
    (define (~string-list xs sep)
      (apply string-append 
             (map ~a (add-between xs sep))))
    (define (~args args) (~string-list args ","))
    (define (~stmt var pr args) 
      (format "~a ~a ~a" var pr (~args args)))
    (define (~instr id stmts v) 
      (define stmts^ (~string-list stmts "\n"))
      (format "instr ~a \n ~a \n out ~a" id stmts^ v)))
  (Stmt : Stmt (S) -> * ()
    [(stmt ,v ,p ,a1 ...) (~stmt v p a1)])
  (Instr : Instr (I) -> * ()
    [(instr ,i ,S1 ... (,o ,v))
     (define str-S1 (map Stmt S1))
     (~instr i str-S1 v)])
  )

(define-parser parse LINST)

;(define (parse s)
  ;(with-output-language (LINST Stmt)
    ;(cond
      ;[(symbol? s) `,s]
      ;[else (error)])))

(pass-instr 
  (parse 
    '(instr 1 (stmt aSin oscil 100 200) 
              (out aSin))))

;(pass-instr (parse '(stmt aSin oscil 100 200)))

