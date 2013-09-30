#lang racket

(require "execute.scm")
(provide (all-from-out "execute.scm"))

(provide measure)
(provide measure!)
(provide partial-measure!)

(define (measure in-register)
  (define (measure-h num in-register res)
    (cond ((coeff? in-register) res)
          (else (define y (prob-sum (register-0s in-register)))
                (cond ((< num y) (measure-h num (register-0s in-register) (append res (list 0))))
                      (else (measure-h (- num y) (register-1s in-register) (append res (list 1))))))))
  (define x (/ (random 10000) 10000))
  (measure-h x (normalise in-register) '()))

(define coeff-prob-sum (λ (reg) (coeff (prob-sum reg))))

(define-syntax partial-measure!
  (syntax-rules ()
    [(partial-measure! x reg) (let ((req-state (measure ((on (remove* x (build-list (no-of-qubits reg) values)) coeff-prob-sum) reg))))
                                (begin 
                                  (set! reg (normalise ((on (append x (remove* x (build-list (no-of-qubits reg) values))) 
                                                            (λ (reg) (tensor (form-reg req-state) (use (λ (x) x) reg req-state)))) reg)))
                                  req-state))]))

(define-syntax measure!
  (syntax-rules ()
    [(measure! x) (let ((a (measure x)))
                    (begin
                      (set! x (form-reg a))
                      a))]))
