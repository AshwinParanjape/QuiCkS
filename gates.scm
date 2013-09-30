#lang racket


;uses the functions from requirements.scm file
(require "requirements.scm")
(provide (all-from-out "requirements.scm"))


;providing the gates made in this file
(provide fredkin)
(provide Pauli-X)
(provide Pauli-Y)
(provide Pauli-Z)
(provide phase-shift)
(provide toffoli)
(provide swap)
(provide hadamard)
(provide CNOT)
(provide control)


;takes no.of controlled qubits and the basic/complex gate to form complex gate(procedeure) and applies as per on-function 
(define (control control-qubits gate)
  (λ (in-register)
    (cond ((check-gate gate (- (no-of-qubits in-register) control-qubits) control-qubits)
           (if (= control-qubits 0) (gate in-register)
               (register (register-0s in-register) ((control (- control-qubits 1) gate) (register-1s in-register))))))))



;checks if the no.of inputs given matches with that of given basic/complex gate.controls for bsic is 0
;gives required input qubits other-wise
(define (check-gate gate num controls)
  ;this defines the no.of inputs of basic gates
  (define check-list (list (cons fredkin 3) (cons Pauli-X 1) (cons Pauli-Y 1) (cons Pauli-Z 1) (cons phase-shift 1)
                           (cons swap 2) (cons toffoli 3) (cons hadamard 1) (cons CNOT 2)))
  (define (checker lst)
    (cond ((null? lst) "Unknown gate")
          ((equal? gate (caar lst)) 
           (if (= num (cdar lst)) #t 
               (begin (error (string-append "The " (if (= controls 0) " " "innermost") " gate requires a register with " 
                                                               (number->string (+ controls (cdar lst))) " qubits")) #f)))
          (else (checker (cdr lst)))))
  (checker check-list))

;checks the input & applies control and swap gates as per on function i.e, controlled-swap acting on 3-qubits
(define (fredkin in-register)
  (cond ((check-gate fredkin (no-of-qubits in-register) 0) ((control 1 swap) in-register))))

;it is quantum equivalent of not,acts on 1 qubit and swaps the probabilities of its states
(define (Pauli-X in-register) 
  (cond ((check-gate Pauli-X (no-of-qubits in-register) 0)(register (register-1s in-register) (register-0s in-register)))))

;it equates rotation of bolch sphere about y-axis by pi-radians
;sets c|1> to -ic|1> , c|0> to ic|0> and swaps the probabilities
(define (Pauli-Y in-register)
  (cond ((check-gate Pauli-Y (no-of-qubits in-register) 0) 
         (register (coeff (* 0-1i (get-coeff in-register '(1)))) (coeff (* 0+1i (get-coeff in-register '(0))))))))


;it equates rotation about z-axis by 180 deg. , it makes |1> state out of phase 
(define (Pauli-Z in-register)
  (cond ((check-gate Pauli-Z (no-of-qubits in-register) 0) 
                      (register (register-0s in-register) 
                                (coeff (* -1 (coeff-val (register-1s in-register))))))))


;acts on 1 qubit and shifts the phase of |1> by given angle-x
(define (phase-shift x)
  (λ (in-register)  (cond ((check-gate phase-shift (no-of-qubits in-register) 0)
                           (register (register-0s in-register) 
                                     (coeff (* (make-rectangular (cos x) (sin x)) (get-coeff in-register '(1)))))))))


;acts on 2 qubits and swaps probabilities of states of (0 1) and (1 0)
(define (swap in-register)
  (cond ((check-gate swap (no-of-qubits in-register) 0)
         (register (register (coeff (get-coeff in-register '(0 0))) (coeff (get-coeff in-register '(1 0))))
                   (register (coeff (get-coeff in-register '(0 1))) (coeff (get-coeff in-register '(1 1))))))))

;it is basically CCNot,acts on 3 qubits ,applies control and basic CNot as per on-function
(define (toffoli in-register)
  (cond ((check-gate toffoli (no-of-qubits in-register) 0) ((control 1 CNOT) in-register)))) 

;it is basic gate.acts on 1 qubit and transforms by given formula
(define (hadamard in-register)
  (cond ((check-gate hadamard (no-of-qubits in-register) 0)
         (register (coeff (/ (+ (get-coeff in-register '(0)) (get-coeff in-register '(1))) (sqrt 2)))
                   (coeff (/ (- (get-coeff in-register '(0)) (get-coeff in-register '(1))) (sqrt 2)))))))

;takes 2 qubits,provided as basic gate.applies control and pauli-X as per on-function
(define (CNOT in-register)
  (cond ((check-gate CNOT (no-of-qubits in-register) 0) ((control 1 Pauli-X) in-register))))
