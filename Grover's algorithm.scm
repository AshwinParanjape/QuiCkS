#lang racket

(require "quantum-simulator.scm")

;w is the required state

;This is the Oracle and we need to count its number of iterations
(define (U-w w)
  (λ(x) (qs ( ! x > ) - 2 * ( *< x ! w > ) * (! w >) )))

(define (U-s s)
  (λ(x) (qs 2 * ( *< s ! x > ) * (! s >) - ( ! x > ))))

(define count 0)
(define (Grovers-algo oracle no-of-elements)
  (define init-reg (normalise (1-register (ceiling (/ (log no-of-elements) (log 2))))))
  (define (Grover-iteration in-reg n)
    (if (> n  0) 
        (begin (set! count (+ count 1))
        (Grover-iteration ((U-s init-reg) (oracle in-reg)) (- n 1)))
        in-reg))
  (Grover-iteration init-reg (ceiling (expt no-of-elements 0.5))))

(define oracle (U-w '(0 1 0 0 0 1 0 0 1 0)))
;(oracle-counter oracle)

(measure (Grovers-algo oracle 1000))
count

