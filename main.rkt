#lang racket/base

(require "ffi.rkt")

(csound-initialize 3)

(define orc #<<EOF
sr=44100
ksmps=32
nchnls=2
0dbfs=1

instr 1 
aout vco2 0.5, 440
outs aout, aout
endin
EOF
)

; Defining our Csound SCO code 
(define sco "i1 0 1")

(let ([cs (csound-create)] ; Create an instance of the Csound object 
      [args '("csound" "test1.csd")]) ; Create args as list

  (csound-set-option cs "-odac") ; Using SetOption() to configure Csound 
                                 ; Note: use only one commandline flag at a time
  (csound-compile-orc cs orc)     ; Compile the Csound Orchestra String
  (csound-read-score cs sco)      ; Compile the Csound SCO String 
  (csound-start cs)               ; When compiling from strings, this call is necessary before doing any performing 
  (csound-perform cs)            ; This call runs Csound to completion
  (csound-stop cs)
  
  (csound-destroy cs))



