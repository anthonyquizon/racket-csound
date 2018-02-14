#lang racket

(provide (all-defined-out))

(struct Signal (op args))
(struct Score (ops))
(struct GEN (id))
(struct Note (sig start duration params))

