#lang racket/gui

(require racket/mpair)

;uses the code written for manipulation of registers
(require "back-end.scm")
(provide (all-from-out "back-end.scm"))

;provides the interface to simulator
(provide f)
(provide f1)
(provide f2)


;creation of frames : 
; one providing the options and tools & othes to allow circuit constuctions and evaluation
 
(define f (new frame%
               [label "Quantum Computer Simulator"]
               [width 700]
               [height 700]
               [x 50]
               [y 50]))
(define f1 (new frame%
                [label "Tools"]
                [width 200]
                [height 700]
                [x 760]
                [y 50]))

;frame to take input
(define f2 (new frame%
                [label "input"]
                [width 200]
                [height 700]
                [x 1032]
                [y 50]
                [stretchable-height #t]
                [enabled #f]))



;standardizes sizes to deal with bitmaps
(define GUI-no-of-qubits 0)
(define image-size-x 60)
(define image-size-y 35)
(define gates-defined 0)
(define init-qubit-x 30 )
(define init-qubit-y 30 )

;creates 2 panel to provide options in f and f1 frames
(define Run-Debug-panel (new horizontal-panel% [parent f]))
(define tools-panel (new vertical-panel% [parent f1]))

;panels in tools-panel for providing bitmaps of gates etc.
(define p1 (new horizontal-panel% [parent tools-panel]))
(define p2 (new vertical-panel% [parent tools-panel]))
(define p3 (new horizontal-panel% [parent tools-panel]))
(define p6 (new horizontal-panel% [parent tools-panel]))
(define p4 (new horizontal-panel% [parent tools-panel]))
(define p5 (new horizontal-panel% [parent tools-panel]))
(define p7 (new horizontal-panel% [parent tools-panel]))


(define (func n)
  (map (λ(string) (new text-field% [label string] [parent f2]))
       (map (λ(state) (string-append "|"(foldr (λ(bit rest) (string-append (number->string bit) rest))
                                                                           ""
                                                                           state) ">" ))
                                        (generate-all-states n))))
(define references '())
(define in-reg (form-reg (build-list 1 (λ(x) 1))))

(new button% [label "done"] [parent f2]
     [callback (lambda (button event) 
                 (set! references (map (lambda (txt-fld) (string->number (send txt-fld get-value))) references))
                 (set! in-reg (pump references)))])


;defined register class providing functions to :
;  draw qubit-images with given no.of qubits

(define register% 
  (class object%
    (super-new)
    (define name "qubit")
    (define image (make-object bitmap% (string-append "images/" name ".png")))
    
    ;draws alligned corresponding to qubits by rounding mouse-click co-ordinates
    (define (draw-image dc image qubit-no) 
      (send dc draw-bitmap image
            init-qubit-x (- (+ (* qubit-no image-size-y) init-qubit-y) (/ image-size-y 2))))
    
    ;called back by on-paint
    (define/public (click-handler dc)
      (begin (draw-image dc image  GUI-no-of-qubits)
             ;(send action-GUI-instance add-GUI-event (λ()(draw-image dc (list-ref image-list (- qubit-no 1)))))
             (set! GUI-no-of-qubits (+ GUI-no-of-qubits 1))
             (set! in-reg (form-reg (build-list GUI-no-of-qubits (λ(x) 1) )))))
    
    
    ;creates a button in p1 panel to represent qubit-bitmap and draws when clicked,
    (new button%
         [label (make-object bitmap% (string-append "images/" name ".png"))]
         [parent p1]
         [min-width 20]
         [min-height 20]
         [callback (λ(button event) (begin (set! current-button this)
                                           (send mcanvas on-paint)
                                           (send t set-value name)
                                           ))])
    (new button% [label "|1>"] [parent p1] [callback (lambda (button event) 
                                              (set! in-reg (form-reg (build-list GUI-no-of-qubits (λ(x) 1)))))])
    (new button% [label "|0>"] [parent p1] [callback (lambda (button event) 
                                              (set! in-reg (form-reg (build-list GUI-no-of-qubits (λ(x) 0)))))])
    (define p8 (new horizontal-panel% [parent p1]))
    (new button% [label "User-Defined"] [parent p8] [callback (lambda (button event) 
                                                     (begin 
                                                       (send f2 enable #t)
                                                       (set! references (func GUI-no-of-qubits))))])
    ))


;an object of above class to implement it
(define register-instance (make-object register%))

;sets current-button to qubit initially
(define current-button register-instance)

(define a-mouse-event
  (new mouse-event% [event-type 'left-up]))

;new my-canvas class defined to handle mouse-click events in the canvas
(define my-canvas%
  (class canvas%
    (define clicked? #f)
    ;function to handle mouse-click event:gets co-ordinates and draws selected-bitmap
    (define/override (on-event event)
      (cond ((and (send event get-left-down) (not clicked?)) 
             (begin (update-coord (send event get-x) (send event get-y))
                    (set! a (+ a 1))
                    (send mcanvas on-paint)
                    (set! clicked? #t)))
            ((and (send event get-left-down) clicked?) '())
            (else (set! clicked? #f))))
    ;(begin (display x) (display " ") (display y) (display #\newline))))
    
    ; Call the superclass init, passing on all init args
    (super-new)
    ))

;an instance of my-canvas% ,provides the canvas to handle all drawing functions to make circuits
(define mcanvas (new my-canvas% 
                     [parent f]
                     [min-width 700]
                     [min-height 500]
                     [style (list  'hscroll 'vscroll)] 
                     [paint-callback                      
                      (lambda (canvas dc) 
                        (send current-button click-handler dc))]))



;checks if control gates are present 
(define control? #f)
;new text-field to take input(number of controlled gates applied on a given gate)
(define control-field (new text-field% [parent p2] [label "Control-qubits"] [init-value "1"] [enabled #f][min-width 40]))
;check-box t activate controlled gates
(define control-box (new check-box% [label "Enable Controlled Gates"][parent p2]
                         [callback (λ(box event) (if (send box get-value)
                                                     (begin 
                                                       (send control-field enable #t)
                                                       (set! control? #t))
                                                     (begin 
                                                       (send control-field enable #f)
                                                       (set! control? #f))
                                                     ))]))


;new class defined to create objects for different basic gates providing buttons,bitmaps and panel of placement
;it avoids the repetition of code for creation,as it creates all instances with one list input
(define gate% 
  (class object%
    
    ;fields requried to initiate buttons and bitmaps
    (init-field gate)
    (init-field (panel '()))
    (init-field (image-names (list)))
    (init-field (name '()))
    
    
    
    (init-field (input-method (λ(x)'())))
    (super-new)
    ;creates bitmaps with given names in list
    (define image-list 
      (map (λ(name) (make-object bitmap% (string-append "images/" name ".png"))) image-names))
    (define gate-no-of-qubits (length image-list))
    (define qubits-put 0)
    (define on-list (mlist '()))
    (define/public (init-gate . exp)
      (set! gate (apply gate exp)))
    (define (draw-image dc image qubit-no) 
      (send dc draw-bitmap image
            (+ (* (+ gates-defined 1) image-size-x) init-qubit-x)
            (- (+ (* qubit-no image-size-y) init-qubit-y) (/ image-size-y 2))
            ))
    ;refreshes the entire simulation system,sets to initial
    (define/public (refresh) (begin (set! qubits-put 0) (set! on-list (mlist `())))) 
    
    (define/public (click-handler dc)
      (let ((qubit-no (cond ((>= (round (/ (- y init-qubit-y) image-size-y)) GUI-no-of-qubits) (- GUI-no-of-qubits 1))
                            ((< (round (/ (- y init-qubit-y) image-size-y)) 0) 0) 
                            (else (round (/ (- y init-qubit-y) image-size-y))))))
        (if (mmember qubit-no on-list ) '()
            (begin 
              (draw-image dc (list-ref image-list qubits-put) qubit-no)
              (send action-GUI-instance add-GUI-event (λ()(draw-image dc (list-ref image-list qubits-put) qubit-no)))
              
              (mappend! on-list (mlist qubit-no))
              (cond ((= qubits-put (- gate-no-of-qubits 1))
                     (begin (set! gates-defined (+ gates-defined 1)) 
                            (send action-GUI-instance add-gate (on (mlist->list (mcdr on-list)) gate))
                            (set! on-list (mlist '()))
                            (send action-GUI-instance add-to-Q)
                            (set! qubits-put 0))) 
                    (else (begin (set! qubits-put (+ qubits-put 1))
                                 )))))))
    
    ;creates buttons in the panel with bitmaps as labels
    (cond ((not (null? name))
           (new button%
                [label (make-object bitmap% (string-append "images/" name ".png"))]
                [parent panel]
                [min-width 20]
                [min-height 20]
                [callback 
                 (λ(button event) 
                   (if control? 
                       (let ((c-q (string->number (send control-field get-value))))
                         (define x (new gate% 
                                        [image-names (append (build-list c-q (λ(x) "c")) image-names)]
                                        [gate (control c-q gate)]
                                        [input-method input-method]))
                         (begin 
                           (set! current-button x)
                           (send t set-value (string-append "Control" (send control-field get-value) name))
                           (set! control? #f)
                           (send control-field enable #f)
                           (send control-box set-value #f)))
                       
                       (begin (input-method this)
                              (set! current-button this)
                              (send t set-value name))))]))))) 
(define p4.1 (new vertical-panel% [parent p4]))
(define p4.1.1 (new horizontal-panel% [parent p4.1]))


;phase-shift defined seperatley to handle call-back to enable and disable
(define phase-shift-button (new gate% [name "Phase-shift"] 
                         [image-names (list "Phase-shift")]
                         [gate phase-shift] 
                         [input-method (λ(gate-obj)
                                         (begin (send feed-button enable #t)
                                                (send ph-shift-input enable #t)
                                                
                                                ))]
                         [panel p4.1.1]))

;text-field to take phase-angle input
(define ph-shift-input (new text-field% 
                            [parent p4.1] 
                            [label "Angle(in rad):pi*"] 
                            [init-value (number->string 1)]
                            [enabled #f]))
;button to feed the value taken from input t othe code
(define feed-button (new button% [label "Feed"] 
                         [parent p4.1.1] 
                         [callback (λ(button event) (send phase-shift-button init-gate (* pi (string->number (send ph-shift-input get-value))))
                                     (send feed-button enable #f)
                                     (send ph-shift-input enable #f))]
                         [enabled #f]))


;initialisation of all buttons of gate% with a list
(define button-list
  (list (new gate% [gate hadamard] [name "Hadamard"][image-names(list "Hadamard")]
             [panel p3])
        (new gate% [name "C-not"] [image-names (list "c" "not" )] [gate CNOT][panel p5])
        (new gate% [name "Toffoli"] [image-names (list "c" "c" "not" )] [gate toffoli][panel p7])
        (new gate% [name "Pauli-X"] [image-names (list "Pauli-X")] [gate Pauli-X][panel p3])
        (new gate% [name "Pauli-Y"] [image-names (list "Pauli-Y")] [gate Pauli-Y][panel p6])
        (new gate% [name "Pauli-Z"] [image-names (list "Pauli-Z")] [gate Pauli-Z][panel p6])
        phase-shift-button
        (new gate% [name "Swap"] [image-names (list "cross" "cross")] [gate swap][panel p5])
        (new gate% [name "Fredkin"] [image-names (list "not" "cross" "cross")] [gate fredkin][panel p7])))


;state define not to perform any action initially
(define a 0)

;class constructed to store the order of action of gates and functions to manipulate 
(define action-GUI-Q%
  (class object%
    (super-new)
    (define action-Q (mlist (λ(in-reg) in-reg)))
    (define GUI-Q (mlist (mlist (λ()'()))))
    (define new-gate '())
    (define new-gate-GUI-events (mlist '()))
    (define/public (add-gate gate) (set! new-gate gate))
    ;adds the new gate constructed to the list
    (define/public (add-GUI-event event) (mappend! new-gate-GUI-events (mlist event)))
    ;updates the action-Q and gui-Q
    (define/public (add-to-Q) (begin
                                (mappend! action-Q (mlist new-gate))
                                (mappend! GUI-Q (mlist new-gate-GUI-events))))
    
    ;evaluates the complete resgister after applied by given cirut and displays
    (define/public (run)   
                           (if (null? action-Q) in-reg
                               ((apply compose (reverse (mlist->list action-Q))) in-reg)))
    (define/public (reverse-run) (let ((in-reg  (form-reg (build-list GUI-no-of-qubits (λ(x)1)))))
                                   (if (null? action-Q) in-reg
                                       ((apply compose (mlist->list action-Q)) in-reg))))
    ;updates the Q-s to initial states after refrersh is called
    (define/public (clear) (begin (set! action-Q (mlist (λ(in-reg) in-reg)))
                                  (set! GUI-Q (mlist (mlist (λ()'()))))))
    
    ))

;text-field to indicate the gate selected
(define t (new text-field% [parent f] [label #f] [init-value "no-image"]))


;initial co-ordinates and functions to update after mouse-click
(define x 0)
(define y 0)
(define (update-coord x-new y-new) 
  (begin (set! x x-new) 
         (set! y y-new)))

;procedure to create buttons required in simulator frame
(define (button-creator name panel action)
  (new button%
       [label name]
       [parent panel]
       [min-width 20]
       [min-height 20]
       [callback action]))


;creates instances of buttons
(begin (button-creator "refresh" Run-Debug-panel (lambda (button event)
                                                   (begin (send t set-value "") (send mcanvas refresh)
                                                          (set! current-button register-instance)
                                                          (set! gates-defined 0)
                                                          (set! GUI-no-of-qubits 0)
                                                          (send action-GUI-instance clear)
                                                          (for-each (λ(button) (send button refresh)) button-list))
                                                   )))
(button-creator "Run" Run-Debug-panel (lambda (button event)
                                        (send t set-value (display-reg (send action-GUI-instance run)))))
(button-creator "Reverse-Run" Run-Debug-panel (lambda (button event)
                                                (send action-GUI-instance reverse-run)))
;(button-creator "Undo" gate-panel (lambda (button event) (send action-GUI-instance undo))



;defined to implement debugging option
(define GUI-debugger% 
  (class object%
    (super-new)
    
    (define prev-button register-instance)
    (define debug-obj '())
    ;creates new-panel with options
    (define debug-panel (new horizontal-panel% [parent Run-Debug-panel]))
    (define name "break-point")
    (define image (make-object bitmap% (string-append "images/" name ".png")))
    ;procedure to draw image at standard positon
    (define (draw-image dc image gate-no) 
      (send dc draw-bitmap image
            (- (+ (* gate-no  image-size-x) init-qubit-x) (/ image-size-x 4)) (- init-qubit-y image-size-y )))
    ;defined to round-off co-ordinates of mouse-click to draw at standard position
    (define/public (click-handler dc)
      (let ((gate-no (cond ((> (round (/  (- x init-qubit-x) image-size-x)) (+ gates-defined 1))  (+ gates-defined 1))
                           ((< (round (/ (- x init-qubit-x) image-size-x)) 1) 1) 
                           (else  (round (/ (- x init-qubit-x) image-size-x))))))
        (begin 
          (draw-image dc image  gate-no)
          
          (send debug-obj add-break-point (- gate-no 1)))))
    
    ;step button to implement debugg : move one step in execution and display current state
    (define step-button (new button% [label "Step"] [parent debug-panel] 
                             [callback (λ(button event) (send debug-obj step))]
                             [enabled #f]))
    
    ;goes upto the desired break point
    (define go-button (new button% [label "Go"] [parent debug-panel] 
                           [callback (λ(button event) (send debug-obj go))]
                           [enabled #f]))
    
    ;it's a check box to enable step and go when debugg is in process
    

    (define debug-box (new check-box%
                           [label "Debug"]
                           [parent debug-panel] 
                           [callback (λ(box event)(if (send box get-value)
                                                      (begin
                                                        (debug-callback)
                                                        (set! debug-obj (send action-GUI-instance run))
                                                        (send debug-obj set-text-field t)
                                                        (set! prev-button current-button)
                                                        (set! current-button this)
                                                        (send step-button enable #t)
                                                        (send go-button enable #t)
                                                        (gui-debug-callback)
                                                        )
                                                      (begin (debug-callback)
                                                             (set! debug-obj '())
                                                             (set! current-button prev-button)
                                                             (send step-button enable #f)
                                                             (gui-debug-callback)
                                                             (send go-button enable #f))))]))
    ))


;instances of above classes to implement its functions
(define debug-inst (new GUI-debugger%))

(define action-GUI-instance (make-object action-GUI-Q%))

;to draw in the canvas
(define (paint dc) (send current-button draw-image))  

(send f show #t)
(send f1 show #t)
(send f2 show #t)