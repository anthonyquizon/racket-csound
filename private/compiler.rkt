#lang nanopass

(module+ test 
  (require rackunit))

(define primitives '(oscil))

(define (Primitive? v) (memq v primitives))
(define (Id? v) (number? v))
(define (PValue? v) (number? v))
(define (Out? v) (equal? v 'out))
(define (Argument? v) (number? v))
(define (Variable? v) ;;TODO control or audio rate 
  (and (symbol? v) (not (Primitive? v)))) 

;;pass define sig
;;pass define inst
;;pass automatic instr id

;(define-language L1
  ;(extends L0)
  ;(terminals 
    ;(- (Id (i))))
  ;(Stmt (S)
    ;(stmt v p a1 ...)))

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
  (Event (E)
    (i pv1 pv2 pv3 pv* ...))
  (Score (Sc)
    (score E1 ...))
  (CSD (C)
    (csd I Sc)))

(define-pass pass-csd : L0 (ir) -> * ()
  (definitions 
    (define (~string-list xs sep)
      (apply string-append 
             (map ~a (add-between xs sep))))
    (define (~args args) (~string-list args ","))
    (define (~stmt var pr args) 
      (format "~a ~a ~a" var pr (~args args)))
    (define (~instr id stmts v) 
      (define stmts^ (~string-list stmts "\n"))
      (format "instr ~a \n ~a \n out ~a" id stmts^ v))
    (define (~i pv1 pv2 pv3 pv*)
      (format "i ~a ~a ~a ~a" pv1 pv2 pv3 (~string-list pv* " ")))
    (define (~score events)
      (~string-list events "\n"))) 

  (Stmt : Stmt (S) -> * ()
    [(stmt ,v ,p ,a1 ...) (~stmt v p a1)])
  (Instr : Instr (I) -> * ()
    [(instr ,id ,S1 ... (,o ,v))
     (define str-S1 (map Stmt S1))
     (~instr id str-S1 v)])
  (Event : Event (E) -> * ()
    [(i ,pv1 ,pv2 ,pv3 ,pv* ...) (~i pv1 pv2 pv3 pv*)])
  (Score : Score (Sc) -> * ()
    [(score ,E1 ...) 
     (define str-E1 (map Event E1))
     (~score str-E1)])
  (CSD : CSD (C) -> * ()
    [(csd ,I ,Sc)
     `(,(Instr I) . ,(Score Sc))])) 

(define-parser parse L0)

(module+ test
 
   (define ex '(csd
                 (instr 1 
                        (stmt aSin oscil 100 200) 
                        (out aSin))
                 (score 
                   (i 1 0 0)
                   (i 1 200 10))))

   (pass-csd (parse ex)))

   ;(check-equal? 
     ;(pass-csd 
       ;(parse ex)
       ;"instr 1\n aSin")))
 
 

