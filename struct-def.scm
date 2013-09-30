#lang racket

(provide register)
(provide register-0s)
(provide register-1s)
(provide register?)
(provide coeff)
(provide coeff-val)
(provide coeff?)
(provide 1st-qubit)
(provide rest-of-list)

(struct register (0s 1s) #:transparent)
(struct coeff (val) #:transparent)
(define 1st-qubit car)
(define rest-of-list cdr)