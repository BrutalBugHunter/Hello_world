#lang racket/gui
(require "legacy-graphics-project.scm")
 (define frame (new frame% [label "Intro"][width 200][height 200]
                    [stretchable-width #f] [stretchable-height #f]))

(define my-canvas%
    (class canvas%
      (define/override (on-event event)
        (send msg set-label (string-append (number->string (send event get-x))"," (number->string (send event get-y)))))
      (define/override (on-char event)
        (define pressed (send event get-key-code))
        (cond ((char? pressed) (send msg set-label (string-append  "key pressed:" (make-string 1 pressed))))))
      ; Call the superclass init, passing on all init args
      (super-new)))
(define sides 0)
(new text-field% [parent frame]
     [label "Enter number of sides  "]
               ; Callback procedure for text-field:
     (callback (lambda (textbox event)
                 ;(if (eq? (send textbox get-value) "Enter number of side here")
                     ;(begin (send textbox set-value "") (display "y"))
                     (set! sides (string->number (send textbox get-value)))))
     (style (list 'single  'horizontal-label))
     (min-width 10)
     ;(vert-margin 50)
     (stretchable-width #f))
     ;(init-value "Enter number of side here"))

(define msg (new message% [parent frame]
                            [label ""]))

;(define rowpanel (new vertical-panel%
;                      [parent frame]
;                      [alignment '(center top)]
;                      [horiz-margin 50]
;                      [spacing 20]))
;(define row1 (new horizontal-panel% 
;                  [parent rowpanel]
;                  [alignment '(left top)]))
;(new combo-field% [label "How would u like to make the polygon?"]
;     [choices (list "plot it pictorially" "Enter the coordinate of points")]
;     [parent frame])

(define method (new choice% [label "How would u like to make the polygon ?  "]
     [choices (list "Plot it pictorially" "Enter the coordinate of points")]
     [parent frame]
     (style (list 'vertical-label))))

(new button% [parent frame]
               [label "Press me to see the power of CG"]
               ; Callback procedure for a button click:
               (callback (lambda (button event)
                           (cond ((= (send method get-selection) 0)
                                  (polygon sides))
                                 ((= (send method get-selection) 1)
                                  (coordinates sides))
                                 )))
                                  ;(display (send method get-selection))))
               (vert-margin 15))

;(define num1 (new button%
;                  [label "1"]
;                  [parent row1]
;                  [min-width 50]
;                  [min-height 50]
;                  [callback (lambda (button event)
;                              (if (equal? (send msg get-label) "0") 
;                                  (send msg set-label "1")
;                                  (send msg set-label (string-append (send msg get-label) "1"))))]))
  (send frame show #t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require graphics/graphics)
(open-graphics)
;(ready-mouse-click w1)
;(ready-mouse-click w1)
(define m1 (lambda (x) (get-mouse-click x )))
;(define m2 (get-mouse-click v1))
;(query-mouse-posn w1)
;((draw-pixel v1)(mouse-click-posn m1));

(define (drawline viewport) (lambda (point1 point2)
                     ((draw-solid-polygon viewport)	 	 	 	 
                      (list (make-posn (-(posn-x point1) 1.5) (- (posn-y point1) 1.5))
                            (make-posn (+ (posn-x point1) 1.5) (+ (posn-y point1) 1.5))
                            (make-posn (+ (posn-x point2) 1.5) (+ (posn-y point2) 1.5))
                            (make-posn (- (posn-x point2) 1.5) (- (posn-y point2) 1.5)))
                      (make-posn 0 0)	 	 	 	 
                      "red")))

(define (polygon s)
  (define listofvertices '())
  (define v1 (open-viewport "name" 797 978))
   (define position (lambda (point)
                   ((draw-solid-ellipse v1) (make-posn (-(posn-x point) 2.5) (- (posn-y point) 2.5)) 5 5)))
  
  (define p1 (mouse-click-posn (m1 v1)))
  (position p1)
  (set! listofvertices
        (cons (cons (posn-x p1) (posn-y p1)) listofvertices))
  (define (polygon-helper counter point1 point2)
    (if (< counter (- s 1))
        (begin 
          (set! listofvertices (cons (cons (posn-x point2) (posn-y point2)) listofvertices))
          (position point2)
               
               ((drawline v1) point1 point2 )
               ((drawline v1) point1 point2 )
               (polygon-helper (+ counter 1) point2  (interesting point2 point2)))
        (begin (set! listofvertices
                     (cons (cons (posn-x point2) (posn-y point2)) listofvertices))
               (position point2)
               ((drawline v1) point1 point2)
               ((drawline v1) point2 p1)
               listofvertices)))
  (define (interesting initial-point point2)
    (define click? (ready-mouse-click v1))
    (if (not (eq? click? #f))
        (begin ((clear-string v1) point2
                                  (string-append
                                   (number->string (posn-x point2))
                                   ","
                                   (number->string (posn-y point2))))
               (mouse-click-posn click?)) 
        (let ((new-point (query-mouse-posn v1)))
          (begin  (cond ((not (and (= (posn-x new-point) (posn-x point2)) (= (posn-y new-point) (posn-y point2))))
                         (begin ((clear-line v1) initial-point point2)
                                ((clear-string v1) point2
                                                   (string-append (number->string (posn-x point2)) "," (number->string (posn-y point2))))
                                (position initial-point)
                                ((draw-line v1) initial-point new-point)
                                ((draw-string v1) new-point
                                                  (string-append (number->string (posn-x new-point)) "," (number->string (posn-y new-point)))))))
                  (interesting initial-point new-point)))))
 
  (polygon-helper 1 p1 (interesting p1 p1)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define x_y (make-vector 2 #f))
(define points (make-vector 100))
(define (coordinates number)
  
  
  (define frame2 (new frame% [label "coordinates"][width 200][height 200]
                    [stretchable-width #f] [stretchable-height #f]))
  (define rowpanel (new vertical-panel%
                      [parent frame2]
                      [alignment '(center top)]
                      [horiz-margin 50]
                      [vert-margin 10]
                      [spacing 20]))
  (define (coordinates-helper counter)
    (cond ((< counter number)
           (begin (define row1 (new horizontal-panel% 
                               [parent rowpanel]
                               [alignment '(left top)]))
             (vector-set! points counter 
                          (vector
                           (new text-field% [parent row1]
                                            [label  (string-append "Point" (number->string (+ counter 1)) ".   " "x-cord  ")]
                                            [min-width 10])
                                (new text-field% [parent row1]
                                            [label "    y-cord   "]
                                            [min-width 10])))
             (coordinates-helper (+ counter 1))))))
  (coordinates-helper 0)

  (new button% [parent frame2]
       [label "Preview"]
       (callback (lambda (button event)
                   (preview-polygon points)))
       (vert-margin 15))
  
  (new button% [parent frame2]
               [label "Show me the position of cameras"]
               (vert-margin 10))
  

(define (preview-polygon points)
  (define v2 (open-viewport "preview" 797 978))
  (define list1 '())
  (define position (lambda (point)
                   ((draw-solid-ellipse v2) (make-posn (-(posn-x point) 2.5) (- (posn-y point) 2.5)) 5 5)))

 (define (polygon-helper counter1 list1)
    (cond ((< counter1 (- number 1))
           (let* ((point1 (make-posn (posn counter1 'x) (posn counter1 'y)))
                  (point2 (make-posn (posn (+ counter1 1) 'x) (posn (+ counter1 1) 'y)))
                  (point1_list (cons (posn-x point1) (posn-y point1)))
                  (point2_list (cons (posn-x point2) (posn-y point2))))
             (if (not (= counter1 (- number 2))) 
                 (begin (position point1)
                        ((draw-line v2) point1 point2)
                        (set! list1 (cons point1_list list1)) 
                        (polygon-helper (+ counter1 1) list1))
                 (begin (position point1)
                        (position point2)
                        ((draw-line v2) point1 point2)
                        ((draw-line v2) point2 (make-posn (posn 0 'x) (posn 0 'y)))
                        (set! list1 (cons point1_list list1))
                        (set! list1 (cons point2_list list1))
                        (display list1)))))))

  (polygon-helper 0 list1))

  (define (posn counter1 x_or_y)
    (if (eq? x_or_y 'x)
        (begin (display (string->number (send (vector-ref (vector-ref points counter1) 0) get-value))) 
      (string->number (send (vector-ref (vector-ref points counter1) 0) get-value)))
           
    (string->number(send (vector-ref (vector-ref points counter1) 1) get-value))))
  
  
  (send frame2 show #t))
  
  

