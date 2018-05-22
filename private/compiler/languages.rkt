#lang nanopass

(provide (struct-out CSound)
         L0 
         L1  
         unparse-L0
         unparse-L1
         )

(struct CSound (orc sco) #:transparent)

(define primitives '(oscil))

(define (Primitive? v) (memq v primitives))
(define (Id? v) (number? v))
(define (PValue? v) (number? v))
(define (Out? v) (equal? v 'out))
(define (Argument? v) (number? v))
(define (Variable? v) ;;TODO control or audio rate 
  (and (symbol? v) (not (Primitive? v)))) 

(define-language L0 
  (entry CSD)
  (terminals 
    (Argument (a))
    (PValue (pv))
    (Id (id))
    (Variable (v))
    (Out (o))
    (Primitive (p)))
  (Stmt (S)
    (stmt v p a1 ...))
  (Instr (I)
    (instr id S1 ... (o v)))
  (Event (Ev)
    (i pv1 pv2 pv3 pv* ...))
  (Expr (Ex)
    I
    Ev)
  (CSD (C)
    (csd Ex* ...)))

(define-language L1
  (extends L0)
  (Expr (Ex)
    (- I)
    (- Ev))
  (Orc (O)
    (+ (orc I* ...)))
  (Sco (Sc)
    (+ (sco Ev* ...)))
  (CSD (C)
    (- (csd Ex* ...))
    (+ (csd O Sc)))) 

