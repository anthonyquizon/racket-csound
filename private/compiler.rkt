#lang nanopass

(module+ test 
  (require rackunit))

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

(define-pass consolidate-orc-and-sco : L0 (ir) -> L1 ()
  (definitions 
    (define (Instr? e)
      (nanopass-case (L0 Instr) e
        [(instr ,id ,S1 ... (,o ,v)) #t]
        [else #f]) )

    (define (Event? e)
      (nanopass-case (L0 Event) e
        [(i ,pv1 ,pv2 ,pv3 ,pv* ...) #t]
        [else #f]) ))

  (Instr : Instr (I) -> Instr ())
  (Event : Event (Ev) -> Event ())
  (CSD : CSD (C) -> CSD ()
     [(csd ,Ex* ...) 
      (let ([I* (map Instr (filter Instr? Ex*))]
            [Ev* (map Event (filter Event? Ex*))])
        `(csd (orc ,I* ...) (sco ,Ev* ...)))]))

(module+ test
   (test-case
     "consolidate-orc-and-sco:"
    (define-parser parse L0)
    (check-equal?
      (unparse-L1 
        (consolidate-orc-and-sco
          (parse 
            '(csd
               (instr 1 
                      (stmt aSin oscil 100 200)
                      (out aSin))
               (i 1 0 0)
               (instr 2 
                      (stmt aSin oscil 200 200)
                      (out aSin))
               (i 2 200 10)))))
      '(csd
         (orc 
           (instr 1 
                  (stmt aSin oscil 100 200) 
                  (out aSin))
           (instr 2 
                  (stmt aSin oscil 200 200) 
                  (out aSin)))
         (sco 
           (i 1 0 0)
           (i 2 200 10))))))

(define-pass output-csd : L1 (ir) -> * ()
  (definitions 
    (define (~string-list xs sep)
      (apply string-append 
             (map ~a (add-between xs sep))))
    (define (~args args) (~string-list args ","))
    (define (~stmt var pr args) 
      (format "~a ~a ~a" var pr (~args args)))
    (define (~instr id stmts v) 
      (define stmts^ (~string-list stmts "\n"))
      (format "instr ~a\n ~a\n out ~a" id stmts^ v))
    (define (~i pv1 pv2 pv3 pv*)
      (string-trim 
        (format "i ~a ~a ~a ~a" pv1 pv2 pv3 (~string-list pv* " "))))
    (define (~orc instrs)
      (~string-list instrs "\n"))
    (define (~sco events)
      (~string-list events "\n"))) 

  (Stmt : Stmt (S) -> * ()
    [(stmt ,v ,p ,a1 ...) (~stmt v p a1)])
  (Instr : Instr (I) -> * ()
    [(instr ,id ,S1 ... (,o ,v))
     (define str-S1 (map Stmt S1))
     (~instr id str-S1 v)])
  (Event : Event (E) -> * ()
    [(i ,pv1 ,pv2 ,pv3 ,pv* ...) (~i pv1 pv2 pv3 pv*)])
  (Orc : Orc (O) -> * ()
    [(orc ,I* ...) 
     (define str-I* (map Instr I*))
     (~orc str-I*)])
  (Sco : Sco (Sc) -> * ()
    [(sco ,Ev* ...) 
     (define str-Ev* (map Event Ev*))
     (~sco str-Ev*)])
  (CSD : CSD (C) -> * ()
    [(csd ,O ,Sc)
     (CSound (Orc O) (Sco Sc))])) 

(module+ test
   (test-case
     "output csd"
    (define-parser parse L1)

    (check-equal?
      (output-csd 
        (parse 
          '(csd
             (orc
               (instr 1 
                      (stmt aSin oscil 100 200) 
                      (out aSin)))
             (sco 
               (i 1 0 0)
               (i 1 200 10)))))
      (CSound "instr 1\n aSin oscil 100,200\n out aSin" 
              "i 1 0 0\ni 1 200 10"))))
 
 

