#lang racket
;LOC 93 without comments
;comments - 29 lines
(require racket/mpair)

(require "requirements.scm")
(provide (all-from-out "requirements.scm"))

(provide gui-debug-callback)
(provide debug?)
(provide debug-callback) 
(provide on)
(provide gate-debugger%)




;This function takes a list of qubit-nos as input and a gate. Its function is to prepare the incomming register so that the last three qubits are in the order as desired by the user. Then it applies the gate over the three qubits and reverses the changes that it made in preparing the register. As the change of the order of inputs is not an easy task in the case of qubits so it has been broken down into easier to inplement functions.

(define (on lst gate)
  ;This function takes a register and swaps the two middle registers out of the four that hang on to it. This manifests exchange opf two qubits.
  (define (swap in-reg)
    (let ((a (register-0s (register-0s in-reg)))
          (d (register-1s (register-1s in-reg)))
          (b (register-1s (register-0s in-reg)))
          (c (register-0s (register-1s in-reg))))
      (register (register a c) (register b d))))
  
  ;This function extends swap to a series if swaps which are used to bubble down a particular qubit.
  (define (bubble-down from to in-reg)
    (cond ((= to 0) in-reg)
          ((= from 0) (let ((swapped (swap in-reg)))
                        (register (bubble-down 0 (- to 1) (register-0s swapped))
                                  (bubble-down 0 (- to 1) (register-1s swapped)))))
          (else (register (bubble-down (- from 1) (- to 1) (register-0s in-reg))
                          (bubble-down (- from 1) (- to 1) (register-1s in-reg))))))
  
  ;This fucntion creates a series of bubble-downs so that the required qubits accordinf to the list given to it are at the bottom 
  (define (permute now-lst req-lst in-reg)     
    (let ((now-depth (- (length now-lst) 1)))
      (cond ((null? req-lst) in-reg)
            ((= (car req-lst) (car now-lst)) (permute (cdr now-lst) (cdr req-lst) in-reg))
            (else (let ((qb-pos (- now-depth (list-pos now-lst (car req-lst)))))
                    (permute (remove (car req-lst) now-lst) (cdr req-lst) (bubble-down qb-pos now-depth in-reg)))))))
  ;This function checks for the normality of a given register before application of the gate, informs the user if not normalized and finally normalizes it before application of the gate
  (define (check-norm in-reg)
    (if (= (my-roundoff (prob-sum in-reg)) 1) in-reg
        (begin (display "The given register is not normalised, hence the gate has been applied on its normalized form and the output is ") (display #\newline)
               (normalise in-reg))))
  
  ;This function is an interface between the given list and permute
  (define (action in-reg)
    (let ((depth (no-of-qubits in-reg))
          (asc (build-list (no-of-qubits in-reg) values)))
      ;This is the reverse permuatation
      (permute (append (reverse lst) (reverse (remove* lst asc))) (reverse asc)
               ;over-all-depth function applies the gate on all the registers at the given depth
               (over-all-depth
                ;This is permutation to achieve the required order of qubits
                (permute (reverse asc) (reverse lst)
                         (check-norm in-reg))
                gate (- depth (length lst))))))
  
  (λ (in-reg)
    ;Handler for debugging
    (if debug?  
        ;A workaround.
        ;It is likely that the user will only use the on functions one after another for debugging
        ;Here initially if a register is provided it constructs a debugger object and returns its reference to the on before it. That on receieves a debugger object and proceeds accordingly. Finally the user has a reference to the debugger object with the same command that would be used for application of a gate on a register.
        ;Thus it an example of poly-morphims, also the user has an additional benifit of maintaining many debuggers at once.
        (if (register? in-reg)
            (make-object gate-debugger% in-reg action)
            (begin (send in-reg add-to-stack action) in-reg))
        ;otherwise it returns a function which takes in a register
        (action in-reg)))
  )

(define gui-debug? #f)
(define debug? #f)
(define (debug-callback) 
  (begin
    (if debug? (set! debug? #f)
     (set! debug? #t))
    debug?))
(define (gui-debug-callback) 
  (begin
    (if gui-debug? (set! gui-debug? #f)
     (set! gui-debug? #t))
    gui-debug?))
;The debugger class which is used to create debuggers for a series of on statements
(define gate-debugger%
  (class object%
    (init in-reg)
    (init 1st-action)

    ;The stack of tasks to be done which are provided to it when called from on function.
    (begin (define action-stack (mlist in-reg 1st-action)) (if gui-debug? (send tf set-value (display-reg in-reg))
                                                               (display(display-reg in-reg))))
    
    ;Determines whether in-reg is shown in intermidiate stages.
    (define show? #f)
    (define position 0)
    
    ;The positions where the user may want to stop.
    (define break-points (list (+ (mlength action-stack) 1)))
    (define tf '())
    (super-new)
    
    (define/public (set-text-field t)
      (set! tf t))
    
    (define/public (add-to-stack action)
      (mappend! action-stack (mlist action)))
    
    (define/public (add-break-point n)
      (set! break-points (sort (remove-duplicates (cons n break-points)) <)))
    
    ;This is the function which steps through the action stack without any prejudices towards showing or not showing.
    (define (meek-stepper)
      (if (null? (mcdr action-stack)) 'done
          (begin
            (set-mcar! action-stack ((mcar (mcdr action-stack)) (mcar action-stack)))
            (set-mcdr! action-stack (mcdr (mcdr action-stack)))
            (cond (show? (if gui-debug? (send tf set-value (display-reg (mcar action-stack)))
                             (display(display-reg (mcar action-stack))))))
            (set! position (+ position 1))
            )))
    
    ;This function runs n by not showing while the meek-stepper steps throucg the actions
    (define (run-till n)
      (cond ((not (= n 0)) (begin (set! show? #f) (meek-stepper) (run-till (- n 1))))))
    
    ;This function runs-till the last but one gate before breakpoint and shows and steps forward
    (define/public (go)
      (if (null? break-points) 'done
          (begin 
            (run-till (- (car break-points) position 1))
            (set! show? #t) (step)
            (set! break-points (cdr break-points)))))
    
    ;Implements meek-stepper into a loud-stepper  
    (define/public (step)
      (begin (set! show? #t) (meek-stepper)))
    ))

