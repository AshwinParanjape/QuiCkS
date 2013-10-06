#lang racket

(require "quantum-simulator.scm")
(provide (all-from-out "quantum-simulator.scm"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;  DEUTSCH - JOZSA ALGORITHM   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define count 0)
;the user gives the function in classical form, i.e. as f(x)=0 or f(x)=1 or f(x)=x or f(x)=1-x, then this converts the function into its quantum mechanical counterpart and then applies the Deutsch-Jozsa algorithm on it.
(define (deutsch-modifier f)
  (if (= (f 0) 0)
      (if (= (f 1) 0) (deutsch-jozsa (λ (reg) (begin (set! count (+ count 1)) reg)))
          (deutsch-jozsa (λ (reg) (begin (set! count (+ count 1)) ((on '(0 1) CNOT) reg)))))
      (if (= (f 1) 1) (deutsch-jozsa (λ (reg) (begin (set! count (+ count 1)) ((on '(1) Pauli-X) reg))))
          (deutsch-jozsa (λ (reg) (begin (set! count (+ count 1)) ((on '(1) Pauli-X) ((on '(0 1) CNOT) reg))))))))

;this function carries out all the quantum gate operations to yield the result. 
(define (deutsch-jozsa f)
  (define in-reg (form-reg '(0 1)))
  (define in-reg1 ((on '(0) hadamard) ((on '(1) hadamard) in-reg)))
  (define in-reg2 ((on '(0 1) f) in-reg1))
  (define in-reg3 ((on '(0) hadamard) ((on '(1) hadamard) in-reg2)))
  (deutsch-helper in-reg3))

;this helper function avoids partial-measure! from altering in-reg3, that would have altered the result if the same command was run multiple times. This function does the partial measurement on the 1st qubit. This is a deterministic algorithm that requires just 1 call to the oracle function as opposed to the 2 calls required classically.
(define (deutsch-helper reg)
  (define x (car (partial-measure! '(0) reg)))
  (begin 
    (if (= x 0) (display "given function is constant") (display "given function is balanced"))
    (display #\newline)
    (string-append " The total number of calls to the oracle function is " (number->string count) 
                   " while classically, there would have been 2 calls each time the algorithm was run")))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  GROVER's ALGORITHM  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;w is the required state

;This is the Oracle and we need to count its number of iterations
(define (U-w w)
  (λ(x) (qs ( ! x > ) - 2 * ( *< x ! w > ) * (! w >) )))

(define (U-s s)
  (λ(x) (qs 2 * ( *< s ! x > ) * (! s >) - ( ! x > ))))

(define (Grovers-algo oracle no-of-elements)
  (define init-reg (normalise (1-register (ceiling (/ (log no-of-elements) (log 2))))))
  (define (Grover-iteration in-reg n)
    (if (> n  0) 
        (begin (set! count (+ count 1))
        (Grover-iteration ((U-s init-reg) (oracle in-reg)) (- n 1)))
        in-reg))
  (Grover-iteration init-reg (ceiling (expt no-of-elements 0.5))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;  FAST FOURIER TRANSFORM  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;this function provides the fast fourier transform of any register, which actually represents a superposition of many wave-functions
(define  (fourier in-reg)
  (define (fourier-h in-reg y)
    (if (= y 0) ((on '(0) hadamard) in-reg)
        ((on (list y) hadamard) (phase-controls (fourier-h in-reg (- y 1)) (- y 1) y))))
  (define (phase-controls reg x y)
    (if (< x 0) reg
        ((on (list x y) (control 1 (phase-shift (/ pi (expt 2 (- (- y x) 1)))))) (phase-controls reg (- x 1) y))))
  (fourier-h in-reg (- (no-of-qubits in-reg) 1)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    TESTING  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;DEUTSCH-JOZSA ALGORITHM
;the following two instructions test whether the function f(x)=1 and f(x)=1-x are balanced or constant. Uncomment (remove the ;) any or both to observe the output
;(deutsch-modifier (lambda (x) 1))
;(deutsch-modifier (lambda (x) (- 1 x)))

;GROVER'S ALGORITHM
;the following instructions define an oracle that corresponds to a search for the state (0 1 0 0 0 1 0 0 1 0), classically, this would have taken 1024 steps because there are 10 qubits, the number of steps taken by the quantum mechanical Grover's algo is displayed at the end... To make it work, uncomment each of the following 4 lines
;(define oracle (U-w '(0 1 0 0 0 1 0 0 1 0)))
;(define ans (Grovers-algo oracle 1000))
;(measure! ans)
;count

;FAST FOURIER TRANSFORM
;apply fourier transform on any register and observe output, for example - uncomment the following
;(fourier (register (coeff 0.6) (coeff 0.8)))
;(fourier (pump (list 1 2 3 4)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;    INCOMPLETE CODE HERE ONWARDS;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;   SHOR's ALGORITHM   ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
;  (define list2 (generate-all-states q))
;  (define list1 (flatten-reg input-reg))