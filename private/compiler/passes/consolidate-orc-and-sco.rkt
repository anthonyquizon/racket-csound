#lang nanopass

(provide consolidate-orc-and-sco)

(require "../languages.rkt")

(module+ test 
  (require rackunit))

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

