#lang racket

(require "execute.scm")
(provide (all-from-out "execute.scm"))

(provide convert-to-matrix)
(provide matrix-to-gate)
(provide unitary-check)
(provide apply-matrix)

;returns the first column of a matrix which has been expressed as a list of rows
(define (firstcol m)
  (map (lambda (x) (list (car x))) m))

;returns all but the first column of a given matrix
(define (lastcols m)
  (map (lambda (x) (cdr x)) m))

;returns the product of a row and a column
(define (dot-product x y)
  (if (null? x) 0 (+ (* (car x) (caar y)) (dot-product (cdr x) (cdr y)))))

;returns the dot product of a row with all columns of the other matrix in the form of a list
(define (vmatmult v m)
  (if (null? (car m))  '()
      (cons (dot-product v (firstcol m)) (vmatmult v (lastcols m)))))

;gives the product of two matrices
(define (matmult m1 m2)
  (foldr (lambda (x y) (cons (vmatmult x m2) y)) 
         '()
         m1))

;returns the matrix in the form of a list of columns
(define (cols m)
  (if (null? (car m)) '()
      (cons (firstcol m) (cols (lastcols m)))))

;produces the transpose of a given matrix, i.e. replaces the 'i'th row by the 'i'th column
(define (transpose m)
  (map (lambda (x) (foldr append '() x)) (cols m))) 

;checks whether a given matrix is unitary or not, i.e. whether its complex-conjugate is its own inverse or not. All matrices that are applied on registers in quantum computing must necessarily be unitary. This is one of the major reasons for the reversibility of all quantum gates
(define (unitary-check m)
  (if (equal? (matmult m (transpose (map (λ (x) (map (λ (y) (conjugate y)) x)) m))) (generate-identity (length m))) #t #f))

;this function converts a gate (which is actually a λ (reg) ...) into a matrix, that when applied on the register, gives the same output as the gate
(define (convert-to-matrix gate no-of-qubits)
  (map (λ (x) (flatten-reg (gate (form-reg x)))) (generate-all-states no-of-qubits)))

;this is the opposite of 'convert-to-matrix'. It converts a matrix into a gate (which is actually a λ (reg) ...) that can be applied on the register
(define (matrix-to-gate mat)
  (λ (in-register) (if (= (length mat) (expt 2 (no-of-qubits in-register)))
                       (pump (car (matmult (list (flatten-reg in-register)) (transpose mat))))
                       "The number of rows in the matrix should equal the number of states in the register")))  

;this function generates the identity matrix with n rows
(define (generate-identity n)
  (if (= n 1) (list (list 1)) (cons (cons 1 (zeroes (- n 1))) (map (λ (x) (cons 0 x)) (generate-identity (- n 1))))))

;this function gives a list of m zeroes
(define (zeroes m)
  (if (= m 0) '()
      (cons 0 (zeroes (- m 1)))))


(define (apply-matrix matrix lst in-register)
  (if (unitary-check matrix)
      ((on lst (matrix-to-gate matrix)) in-register)
      "In quantum computation, all matrices need to be unitary. Please re-check the given matrix."))



