#lang racket

(require "back-end.scm")
(provide (all-from-out "back-end.scm"))

(provide oracle-counter)
(provide qs)
(provide !)
(provide *<)


(define-syntax *<
  (syntax-rules ( ! >)
    [(< reg1 ! reg2 >) (register-dot-product (! reg1 >) (! reg2 >))]))
(define-syntax !
  (syntax-rules ( > )
    [( ! reg > ) (qs (if (register? reg) reg
                         (form-reg reg)) )]))
(define-syntax oracle-counter
  (syntax-rules ( )
    [(oracle-counter oracle) 
    (begin (define counter 0)
           (set! oracle (λ(a)
                         (begin (set! counter (+ counter 1))
                                (oracle a)))))
           ]))

(define-syntax qs
  (syntax-rules (! < > -> <- = + - * /)
    [(qs ! expr-var > -> expr ...) (λ(expr-var) (qs expr ...))]
    [(qs var = expr ...)  (define var (qs expr ...))]
    [(qs + expr1 expr2) (handle-op + expr1 expr2)]
    [(qs - expr1 expr2) (handle-op - expr1 expr2)]
    [(qs * expr1 expr2) (handle-op * expr1 expr2)]
    [(qs / expr1 expr2) (handle-op / expr1 expr2)]
    [(qs expr) (if (list? expr)
                  (if (null? (cdr expr)) (car expr) (error "Undefined Usage : remind the developer to handle this") )
                  expr)]
    [(qs op expr) (if (eq? - op) (qs * -1 (qs expr)) (op (qs expr)))]
    [(qs expr ... )
     (begin
       (define (op-to-register-op op exp1 exp2)
  (cond ((or (eq? op +) (eq? op -)) (state-wise (λ(x y)(coeff (op x y))) exp1 exp2))
        ((eq? op *) (tensor exp1 exp2))
        (else (op exp1 exp2))))
       (define (b4-after-last-occurence lst predicate)
         (define (helper lst predicate)
           (cond ((null? lst) (list '() '()))
                 ((predicate (car lst)) (cons '() (cdr lst)))
                 (else (let ((nxt (helper (cdr lst) predicate)))
                         (cons (cons (car lst) (car nxt))(cdr nxt))))))
         (let ((old-ans (helper (reverse lst) predicate)))
           (cons (reverse (cdr old-ans)) (reverse (car old-ans)))))
       (define (ellipsis->lst . exp) exp)
       (define (handle-op op exp1 exp2)
         (let ((final-exp1 (qs exp1))
               (final-exp2 (qs exp2)))
           (cond ((and (register? final-exp1) (register? final-exp2)) (op-to-register-op op exp1 exp2))
                 ((and (number? final-exp1) (number? final-exp2)) (op exp1 exp2))
                 (else (if (or (equal? op *) (equal? op /))
                           (over-all (if (register? exp1) exp1 exp2) (λ(in-coeff-val) (coeff (op in-coeff-val (if (number? exp1) exp1 exp2)))))
                           (op exp1 exp2))))))
      
       (define (infix->prefix precedence-order)
         (define (parse exp m-pre-ord)
           (cond ((null? (cdr exp)) (qs (car exp)))
                 ((null? m-pre-ord) (qs exp))
                 (else (let ((exp1 (b4-after-last-occurence exp (λ(x)(equal?  x (car m-pre-ord))))))
                         (if (equal? (cdr exp1) exp) (parse exp (cdr m-pre-ord))
                             (handle-op (car m-pre-ord) (parse (car exp1) precedence-order) (parse (cdr exp1) precedence-order)))))))
         (λ( exp) (parse exp precedence-order)))
       ((infix->prefix (list + - * /)) (ellipsis->lst expr ...)))]))