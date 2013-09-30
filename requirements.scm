#lang racket

(provide use)
(provide prob) 
(provide my-roundoff)
(provide no-of-qubits)
(provide over-all-depth)
(provide form-reg)
(provide 1-register)
(provide flatten-reg)
(provide pump)
(provide list-pos)
(provide get-coeff)
(provide over-all)
(provide tensor)
(provide state-wise)
(provide prob-sum)
(provide normalise)
(provide generate-all-states)
(provide display-reg)
(provide use)
(provide register-dot-product)
(provide 0-register)
(provide 1-register)

(require "struct-def.scm")
(provide (all-from-out "struct-def.scm"))

;This function gives the dot-product of two registers, similar to the dot product of two wave-functions, wherein the orthogonal states multiply to give zero.
(define (register-dot-product reg1 reg2)
  (define (helper lst1 lst2)
    (if (or (null? lst1) (null? lst2)) '()
        (cons (* (car lst1) (car lst2)) (helper (cdr lst1) (cdr lst2)))))
  (foldr + 0 (helper (flatten-reg (normalise reg1)) (flatten-reg (normalise reg2)))))

;It returns the coefficient of any state in a particular register. The square of the coefficient is an indicator of that state's probability of occurence in a measurement of the register
(define (get-coeff in-register state)
  (coeff-val (use (λ (coeff-of-state) coeff-of-state) in-register state)))

;the 'use' function applies the given function 'f' over every state in the given register
(define (use f in-register state)
  (cond 
    ((null? state) in-register)
    ((register? in-register) (use f (cond ((= (1st-qubit state) 0) (register-0s in-register))
                                          ((= (1st-qubit state) 1) (register-1s in-register))
                                          (else (error "invalid value passed as bit")))
                                  (rest-of-list state)))
    ((coeff? in-register) (f (coeff-val in-register)))
    (else (error "Invalid Structure"))))

;This function returns the entire register after modifying the given state according to function f
(define (mutate f in-register state)
  (cond ((coeff? in-register) (coeff (f (coeff-val in-register))))
        ((null? state) (f in-register))
        ((register? in-register) (cond ((= (1st-qubit state) 0)
                                        (register (mutate f (register-0s in-register) (rest-of-list state))
                                                  (register-1s in-register)))
                                       ((= (1st-qubit state) 1) 
                                        (register (register-0s in-register)
                                                  (mutate f (register-1s in-register) (rest-of-list state))))
                                       (else (error "invalid value passed as bit"))))       
        (else (error "Invalid Structure"))))

;it returns the register after modifying all states according to function f. The function 'f' should return a coeff for proper functioning.
(define (over-all in-register f)
  (over-all-depth in-register (λ(x)(f (coeff-val x))) (no-of-qubits in-register)))

;this returns the tensor product of any number of registers provided as arguments. The tensor product produces an entanglement of states.
(define (tensor r1 r2 . r3) 
  (foldr (λ(r1 r2) (over-all r1 (λ(coeff-r1) (over-all r2 (λ(coeff-r2) (coeff (* coeff-r1 coeff-r2 )))))))
         (over-all r1 (λ(coeff-r1) (over-all r2 (λ(coeff-r2) (coeff (* coeff-r1 coeff-r2 ))))))
         r3))

;'over-all-depth' applies the given function 'f' at a particular depth in the given register. This finds use in the 'on' function given in "execute.scm"
(define (over-all-depth in-register f depth)
  (cond ((= depth 0) (f in-register))
        ((register? in-register) (register (over-all-depth (register-0s in-register) f (- depth 1))
                                           (over-all-depth (register-1s in-register) f (- depth 1))))
        ((coeff? in-register) (f (coeff-val in-register)))
        (else (error "Invalid Structure"))))

;this function applies f on the corresponding states of the two given registers and returns a single coeff for that state in the output register
(define (state-wise f r1 r2)
  (cond ((and (coeff? r1) (coeff? r2)) (f (coeff-val r1) (coeff-val r2)))
        ((or (coeff? r1) (coeff? r2)) (error "Different length registers"))
        (else (register (state-wise f (register-0s r1) (register-0s r2))
                        (state-wise f (register-1s r1) (register-1s r2))))))

;this returns the no-of-qubits in a register. The number of states is equal to (expt 2 no-of-qubits)
(define (no-of-qubits register)
  (if (coeff? register) 0
      (+ 1 (no-of-qubits (register-0s register)))))

;Given the magnitude of the coefficient of a state, this function returns the rounded off value of its square, representative of the state's probability of occurence
(define (prob in-coeff)
  (my-roundoff (expt (magnitude (coeff-val in-coeff)) 2)))

;it rounds off the numbers, essentially a workaround for making things like (expt (/ 1 (sqrt 2)) 2) turn into 0.5 and not 0.49999999, for ease of normalisation check later
(define (my-roundoff frac)
  (define x (* 10000 frac))
  (if (< (- (ceiling x) x) 0.0001) (/ (ceiling x) 10000) frac))

;creates a register with specified number of qubits that has all coefficients as 0
;This is useful as an identity element for functions like foldr
(define (0-register no-of-qubits)
  (if (= no-of-qubits 0) (coeff 0)
      (let ((next-reg (0-register (- no-of-qubits 1))))
        (register next-reg next-reg))))

;creates a register with specified number of qubits that has all coefficients as 1, together with normalise function, this can be used to create a uniform register
(define (1-register no-of-qubits)
  (over-all (0-register no-of-qubits) (λ (x) (coeff (+ x 1)))))

;creates a register with all coefficients 0 except for the one specified as parameter
(define (form-reg state)
  (mutate (λ(x) 1) (0-register (length state)) state))

;returns the position of an element in a given-list
(define (list-pos list ele)
  (let ((rem-list (member ele list)))
    (if (list? rem-list) (- (length list) (length rem-list))
        #f)))

;returns the coefficients of all states in a register in the form of a list
(define (flatten-reg in-reg)
  (define (helper in-reg lst)
    (cond ((coeff? in-reg) (cons (coeff-val in-reg) lst))
          ((register? in-reg) (helper (register-0s in-reg) (helper (register-1s in-reg) lst)))
          (else (error "Unknown structure"))))
  (helper in-reg '()))

;this is the opposite of flatten-reg, it returns a register created using the coefficients of all states given in the form of a list
(define (pump lst)
  (if (null? (cdr lst)) (coeff (car lst))
      (let ((len (/ (length lst) 2)))
        (register (pump (take lst len)) (pump (list-tail lst len))))))

;this function normalises a register, so that the sum of probabilities of occurence of all its states becomes 1
(define (normalise reg)
  (define k (sqrt (prob-sum reg)))
  (define (helper r)
    (cond ((register? r) (register (helper (register-0s r)) (helper (register-1s r))))
          ((coeff? r) (coeff (/ (coeff-val r) k)))))
  (helper reg))

;returns the sum of probabilities of occurence of all states in a given register
(define (prob-sum in-register)
  (cond ((register? in-register) (+ (prob-sum (register-0s in-register)) (prob-sum (register-1s in-register))))
        ((coeff? in-register) (prob in-register))))

;returns a list of all the states in a register of 'n' number of qubits
(define (generate-all-states n)
  (if (= n 0) '(())
      (let ((lst (generate-all-states (- n 1))))
        (append (map (λ(x) (cons 0 x)) lst) (map (λ(x) (cons 1 x)) lst)))))

;given two list of strings, it returns a string which has all corresponding elements of the two lists appended together
(define (zip-strings l1 l2)
  (if (or (null? l1) (null? l2)) '()
      (cons (string-append (car l1) (car l2)) (zip-strings (cdr l1) (cdr l2))))) 

;brings the register into the coefficient-braket notation, e.g. it converts (form-reg '(0)) into "1|0> + 0|1>"
(define (display-reg in-reg)
  (foldr (λ(s1 rest) (string-append s1 rest))
         ""
         (add-between (zip-strings (map (λ(num) (number->string num)) (flatten-reg in-reg)) 
                                   (map (λ(state) (string-append "|"(foldr (λ(bit rest) (string-append (number->string bit) rest))
                                                                           ""
                                                                           state) ">" ))
                                        (generate-all-states (no-of-qubits in-reg)))) " + ")))

;(define (binary->decimal state)
;  (foldr (λ(bit rest) (+ (* rest 2) (if (= bit 1) ) 0 (reverse state))



