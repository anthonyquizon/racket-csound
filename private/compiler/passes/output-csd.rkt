#lang nanopass

(provide output-csd)

(require "../languages.rkt")

(module+ test 
  (require rackunit))

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

