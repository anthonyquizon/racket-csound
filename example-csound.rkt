#lang racket/base

(require "main.rkt")

(define orc #<<EOF
sr = 44100
kr = 4410
ksmps = 10
nchnls = 1

        instr 4
a1      oscil   10000, 440, 1
        out     a1
        endin

        instr 5
a1      oscil   10000, 550, 1
        out     a1
        endin
EOF
)

(define sco #<<EOF 
f1  0   4096    10 1  ; use GEN10 to compute a sine wave
f1  0   4096    10 1  ; use GEN10 to compute a sine wave

;ins    strt    dur
i5      0       4

e                     ; indicates the end of the score
EOF 
)


(run orc sco)


