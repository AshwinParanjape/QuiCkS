#lang racket
(require "quantum-simulator.scm")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   SHOR's ALGORITHM   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this function provides the fast fourier transform of any register, which actually represents a superposition of many wave-functions
;(define  (fourier in-reg)
;  (define (fourier-h in-reg y)
;    (if (= y 0) ((on '(0) hadamard) in-reg)
;        ((on (list y) hadamard) (phase-controls (fourier-h in-reg (- y 1)) (- y 1) y))))
;  (define (phase-controls reg x y)
;    (if (< x 0) reg
;        ((on (list x y) (control 1 (phase-shift (/ pi (expt 2 (- (- y x) 1)))))) (phase-controls reg (- x 1) y))))
;  (fourier-h in-reg (- (no-of-qubits in-reg) 1)))
;
;(define (exptmod x y n)
;  (cond ((= y 0) (modulo 1 n))
;        ((= (modulo y 2) 0) (modulo (exptmod (* x x) (quotient y 2) n) n))
;        (else (modulo (* (exptmod (* x x) (quotient y 2) n) x) n))))
;
;(define (is-prime n)
;  (define (prime-h n i)
;    (define p (random n))
;    (cond ((= p 0) (prime-h n i))
;          ((= i 30) #t)
;          ((not (= (exptmod p (- n 1) n) 1)) #f)
;          (else (prime-h n (+ i 1)))))
;  (prime-h n 0))
;
;(define (mygcd a b)
;  (if (= b 0) a
;      (mygcd b (remainder a b))))
;
;(define (Shor-factorize n)
;  (define (the-else-part)
;    (define a (random n))
;    (cond ((not (= (gcd n a) 1)) (display (string-append "A factor of n is " (number->string (gcd n a)))))
;          (else (define r (period-finder a n))
;                (if (or (= (remainder r 2) 1) (= (remainder (+ 1 (expt a (/ r 2))) n) 0))
;                    (the-else-part)
;                    (display (string-append "The factor of " (number->string n) " is " (number->string (mygcd n (+ 1 (expt a (/ r 2)))))))))))
;  (cond ((is-prime n) "Please enter an odd, composite number to test Shor's algorithm")
;        ((integer? (/ n 2)) (begin (display (string-append "One of the factors is 2, so now we shor-factorize " (number->string (/ n 2))))
;                                   (display #\newline)
;                                   (Shor-factorize (/ n 2))))
;        (else (the-else-part))))
;
;(define (period-finder a n)
;  (define q (ceiling (/ (log n) (log 2))))
;  (define input-reg (tensor (normalise (1-register q)) (register (coeff 1) (coeff 0)))) 
;  (define output-reg (tensor (normalise (1-register q)) (register (coeff 1) (coeff 0))))
;  
(define
(define f 
(define (f1 f in-reg)
  (let ((size (no-of-qubits in-reg)))
    (let ((lst-of-states (generate-all-states size)))
      (foldr (λ(state rest) (qs (get-coeff in-reg state) * (! state >) * (! (f state) >) + rest)) (0-register (* 2 size)) lst-of-states))))


