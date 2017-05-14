#lang racket
(require "vertex.rkt")
(require "edge.rkt")
(require "Polygon3.rkt")
(require "coloring.rkt")
(require "monotone.rkt")
(require data/gvector)
(provide (all-defined-out))
;(define polygon%
;  (class object%
;    (super-new)
;    (init-field size)
;    (init-field vec )))
;
;(define vertex%
;  (class object%
;    (super-new)
;    (init-field x-coor)
;    (init-field y-coor)
;    
;    (define/public (vertex-y-coor) y-coor)
;    (define/public (vertex-x-coor) x-coor)))
;
;(define edge%
;  (class object%
;    (super-new)                                                         ;;;;;;;;;;;;;;;;counterclockwise
;    (init-field first-vertex)
;    (init-field second-vertex)
;    (init-field index-first-vertex-in-vertex-vector)
;    (init-field index-second-vertex-in-vertex-vector)
;    ))
;
;(define monotone%
;  (class polygon%
;    (super-new)
;    (inherit-field size vec)
;    ;(init-field edge-vec)
;    ))
;    
;
;(define node%
;  (class polygon%
;    (super-new)
;    (inherit-field size vec) 
;    (field [colored-count 0])
;    (field [not-colored-yet vec])   ;; vector of vertices initialized in super class
;    (field [colors-possible (if (= size 3) '(1 2 3) '(1 2 3 4))])))
;    




(define (convex-quadrangulation monotone-polygon)
  (let* [(ordered-indices (send monotone-polygon vec-sorted-by-y-indices))       ;;ordered vertices is made up of indices sorted by y coor if y coor same then by x-coor
         (ordered-vertices (vector-map (lambda (x) (vector-ref (get-field vec monotone-polygon)
                                                               x))
                                         ordered-indices))]
    (quadrangulate2 ordered-vertices)))
(define (quadrangulate2 ordered-vertices) 
  (let* [(right-points1 (get-points 1 ordered-vertices ))
         (left-points1 (get-points 0 ordered-vertices ))
         (right-points2 (vector-sort  right-points1 <  #:key (lambda (x) (get-field y-coor x))))
         (left-points2  (vector-sort  left-points1  <  #:key (lambda (x) (get-field y-coor x))))
         (right-points (vector-sort  right-points2 <= #:key (lambda (x) (get-field x-coor x))))
         (left-points  (vector-sort  left-points2 >= #:key (lambda (x) (get-field x-coor x)))) 
         (reflex-left-points (get-reflex-points left-points))
         (reflex-right-points (get-reflex-points  right-points))
         (merge-reflex-points (merge reflex-left-points reflex-right-points))]
    (display (vector-map (lambda (x)
                           (cons (get-field x-coor x)
                                 (get-field y-coor x)))
                                 
                           right-points))
    (display (vector-map (lambda (x)
                           (cons (get-field x-coor x)
                                 (get-field y-coor x)))
                                 
                           merge-reflex-points))
    (define (helper rrl-index lll-index mrl-index list)
      (if (= (+ mrl-index 2) (vector-length merge-reflex-points))
          (list->vector list)
          (if (equal? (vector-ref reflex-right-points rrl-index) (vector-ref merge-reflex-points mrl-index))
              (begin (set! list (cons (make-object node% 4 (vector (vector-ref right-points (+ (* 2 rrl-index) 1))
                                                       (vector-ref right-points (+ (* 2 rrl-index) 2))
                                                       (vector-ref right-points (+ (* 2 rrl-index) 3))
                                                       (vector-ref left-points (+ (* 2 lll-index) 1))))
                                      list))
                     (helper (+ rrl-index 1) lll-index (+ 1 mrl-index) list))
              ;(= (vector-ref reflex-left-points lll-index) (vector-ref merge-reflex-points mr-index))
              (begin (set! list (cons (make-object node% 4 (vector (vector-ref left-points (+ (* 2 lll-index) 1))
                                                       (vector-ref left-points (+ (* 2 lll-index) 2))
                                                       (vector-ref left-points (+ (* 2 lll-index) 3))
                                                       (vector-ref right-points (+ (* 2 rrl-index) 1))))
                                      list))
                     (helper rrl-index (+ lll-index 1) (+ 1 mrl-index) list)))))
    (vector-append (helper 0 0 0 '()) (vector (make-object node% 4 (vector (vector-ref right-points 0)
                                                            (vector-ref left-points 0)
                                                            (vector-ref left-points 1)
                                                            (vector-ref right-points 1)))))))
(define (get-points num vec)                    ;;it returns a vector
  (let* ((start-vertex1 (vector-ref vec 0))
         (start-vertex2 (vector-ref vec 1)))
        
    (define (helper sign vertex index list)
      (if (= index  (vector-length vec))
          (list->vector (reverse list))
          (if (sign (get-field x-coor vertex) (get-field x-coor (vector-ref vec index)))
              (helper sign vertex (+ index 1) list)
              (begin (set! list (cons (vector-ref vec index ) list))
                     (helper sign vertex (+ index 1) list)))))
      (cond ((= num 1)
          (let ((start-right-vertex (get 1 start-vertex2 start-vertex1)))
            (helper > start-right-vertex 0 '())))
          ;(let ((start-left-vertex (get 0 start-vertex2 start-vertex1)))
            ;(display start-left-vertex) 
            ((= num 0)
             (helper < (get 0 start-vertex2 start-vertex1) 0 '())))))

(define (get num ver2 ver1)    ;;a subroutine ver2 ver1 start 
  (cond ((and (= num 1) (< (get-field x-coor ver1) (get-field x-coor ver2))) ver2)
        ((and (= num 1) (> (get-field x-coor ver1) (get-field x-coor ver2))) ver1)
        ((and (= num 0) (< (get-field x-coor ver1) (get-field x-coor ver2))) ver1)
        ((and (= num 0) (> (get-field x-coor ver1) (get-field x-coor ver2))) ver2)))


(define (get-reflex-points points)   ;; it returns a vector
  (define (helper index list)
    (if (<  index (vector-length points))
        (helper (+ index 2) (cons (vector-ref points index) list))
        (list->vector (reverse list))))
  (helper 1 '()))

  
(define (merge reflex-left-points reflex-right-points)  ;; it returns vector of left reflex and right reflex vertices ordered by y coordinates
  (let ((without-sort-merge (vector-append reflex-right-points reflex-left-points)))
    (define (vec-sorted-by-y-indices without-sort-merge)                                         ;;;  we need vertices sorted by y coordinate so there it is. Actualy it is
      (vector-map (lambda (x) (vector-ref without-sort-merge x)) (vector-map                                                            ;;;;;;;;; index of the vectors in the vector vec
       (λ(x) (cdr x))                                        ;;;;; Contact 8291446857
       (vector-sort
        (list->vector (zip (vector->list without-sort-merge)
                           (list-upto (vector-length without-sort-merge))))
        >= 0 (vector-length without-sort-merge) #:key (lambda(x) (send (car x) vertex-ycoor))))))
    (vec-sorted-by-y-indices without-sort-merge)))




