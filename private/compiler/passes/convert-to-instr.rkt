#lang nanopass

(provide consolidate-orc-and-sco)

(require "../languages.rkt")

(module+ test 
  (require rackunit))

(define-pass convert-to-instr : LTop0 (ir) -> L1 ()
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
     "convert-to-instr"
    (define-parser parse LTop0)

    (check-equal?
      (unparse-L1 
        (consolidate-orc-and-sco
          (parse 
            '(csd
               (define a (oscil 100 200)) 
               (define (b x) (oscil x 300)) 
               (a 0 0)
               (a 10 20)
               ((b 200) 0 20)
               ((b 300) 0 30)))))
      '(csd
         (sig 1 (oscil 100 200))
         (sig 2 (oscil 200 300))
         (sig 3 (oscil (sig-ref 3) 300))

         ((sig-ref 1) 0 0)
         ((sig-ref 1) 10 20)
         ((sig-ref 2) 0 20)
         ((sig-ref 3) 0 30)
         ))
    



    ))

