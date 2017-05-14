#lang racket

(require "Polygon.rkt")
(require "vertex.rkt")
(require "edge.rkt")
(require "monotone.rkt")
(require data/gvector)

(provide (all-defined-out))

(define (find-within-range vertical-sorted-by-x x1 x2)          ;;;;;;;;;;;;these are also consed at end with x coordnates!!!
  (vector-map (λ(x) (cdr x))
              (list->vector (filter (λ(x) (and (< (cdr x) x2) (> (cdr x) x1))) (vector->list vertical-sorted-by-x)))))

(define (break-vector-into-2 vector index1 index2)
  (define mini (min index1 index2))
  (define maxi (max index1 index2))
  (define vec1 (vector-take vector (+ 1 mini)))
  (define vec2 (vector-drop (vector-take vector (+ 1 maxi)) mini))
  (define vec3 (vector-drop vector maxi))
  (cons vec2 (vector-append vec3 vec1)))
  
(define (gvector-append gv1 gv2) 
  (define len (gvector-count gv2))
  (for [(i (list-upto len))]
    (gvector-add! gv1 (gvector-ref gv2 i)))
  gv1)

(define (modulus x)
  (if (>= x 0)
      x
      (- x)))

(define orthogonal%
  (class polygon%
    ;(init-field edge-vec)  ;;edge vec is array of which each element is orthogonal edge%
    (super-new)
    (inherit-field size vec edge-vec)
    (set! edge-vec                               
          (vector-append
           (build-vector (- size 1)
                         (λ(i)
                           (make-object orthogonal-edge%
                             (vector-ref vec i)                  
                             (vector-ref vec (+ i 1))
                             i
                             (+ i 1))))
           (make-vector 1 (make-object orthogonal-edge%
                            (vector-ref vec (- size 1))
                            (vector-ref vec 0)
                            (- size 1)
                            0))))
    (field [vertical-edges (make-gvector)])
    (field [horizontal-edges (make-gvector)])
    (field [possible-iters '()])

    (for [(i (list-upto size))]
      (let* [(this-edge (vector-ref edge-vec i))
             (this-edge-type (send this-edge edge-type))] 
        (cond [(equal? this-edge-type "Vertical Edge")
               (gvector-add! vertical-edges this-edge)]
              [(or (equal? this-edge-type  "Top Peak")
                   (equal? this-edge-type "Bottom Peak")
                   (equal? this-edge-type  "Top Edge")
                   (equal? this-edge-type "Bottom Edge"))
               (gvector-add! horizontal-edges this-edge)])))
    
    (set! vertical-edges (gvector->vector vertical-edges))
    (set! horizontal-edges (gvector->vector horizontal-edges))

    (define h-size (vector-length horizontal-edges))
    (define v-size (vector-length vertical-edges))

    (define horizontal-edge-sorted-by-y
      (vector-map
       (λ(k) (car k))
       (vector-sort
        (build-vector h-size (λ(i) (let [(the-edge (vector-ref horizontal-edges i))] (cons the-edge (get-field y-first the-edge)))))
        > 0 h-size #:key (λ(x) (cdr x)))))

    ;(display (vector-map (λ(x) (send x edge-type)) horizontal-edge-sorted-by-y))

    (define vertical-edge-sorted-by-x             ;;;;;;;;;pairs of edge, x coordinate
      (vector-sort
       (build-vector v-size (λ(i) (let [(the-edge (vector-ref vertical-edges i))] (cons the-edge (get-field x-first the-edge)))))
       > 0 v-size #:key (λ(x) (cdr x))))
                                    
      
      
      
    (for [(i (list-upto h-size))
          #:break (let* [(this-edge (vector-ref horizontal-edge-sorted-by-y i))]
                    (or (equal? (send this-edge edge-type) "Top Peak")
                        (equal? (send this-edge edge-type) "Bottom Peak")))]
      (set! possible-iters (cons i possible-iters)))           ;;;;;;;;;;;;;;;;;possible iters is just indices as again :D

    
          
        
        
    (define/override (break-to-monotone)
      ;(define highest-peak (vector-ref horizontal-edge-sorted-by-y (+ 1 (car possible-iters))))
    ;(define/public (break-to-monotone)
      (define incomplete #t)
      (define answer #f)
      (if (< (+ 1 (car possible-iters)) (vector-length horizontal-edge-sorted-by-y))
          (let*[(highest-peak (vector-ref horizontal-edge-sorted-by-y (+ 1 (car possible-iters))))]
            (begin
              (if (equal? (send highest-peak edge-type) "Bottom Peak")
                  (for [(i possible-iters)
                        #:when incomplete]
                    (let* [(edge-above (vector-ref horizontal-edge-sorted-by-y i))
                           (type (send edge-above edge-type))]
                      (if (or (equal? type "Top Peak") (equal? type "Top Edge"))
                          (cond [(visible-top-to-bottom-peak? edge-above highest-peak)
                                 (let*[(quad1 (get-field first-vertex highest-peak))
                                       (quad2 (get-field second-vertex highest-peak))
                                       (quad3 (get-field first-vertex edge-above))
                                       (quad4 (get-field second-vertex edge-above))
                                       (index1 (get-field index-first-vertex-in-vertex-vector highest-peak))
                                       (index2 (get-field index-second-vertex-in-vertex-vector highest-peak))
                                       (index3 (get-field index-first-vertex-in-vertex-vector edge-above))
                                       (index4 (get-field index-second-vertex-in-vertex-vector edge-above))
                                       (cons1 (break-vector-into-2 vec index1 index4))
                                       (cons2 (break-vector-into-2 vec index2 index3))
                                       (another-polygon1 (if (or (equal? (vector-ref (car cons1) 1) (vector-ref vec index2))
                                                                 (equal? (vector-ref (car cons1) 1) (vector-ref vec index3)))
                                                             (cdr cons1)
                                                             (car cons1)))
                                       (another-polygon2 (if (or (equal? (vector-ref (car cons2) 1) (vector-ref vec index4))
                                                                 (equal? (vector-ref (car cons2) 1) (vector-ref vec index1)))
                                                             (cdr cons2)
                                                             (car cons2)))
                                       (quadrilateral (vector quad1 quad2 quad3 quad4))
                                       (first-gvector (send
                                                       (make-object orthogonal% (vector-length another-polygon1) another-polygon1)
                                                       break-to-monotone))
                                       (second-gvector (send
                                                        (new orthogonal% [size (vector-length another-polygon2)] [vec another-polygon2])
                                                        break-to-monotone))]
                                   (begin 
                                     (set! incomplete #f)
                                     (set-field! is-special-vertex quad1 #t)
                                     (set-field! is-special-vertex quad2 #t)
                                     (set-field! is-special-vertex quad3 #t)
                                     (set-field! is-special-vertex quad4 #t)   
                                     (gvector-add! first-gvector (new monotone% [size 4] [vec quadrilateral])) 
                                     (set! answer (gvector-append first-gvector second-gvector))))])
                          (cond [(visible-bottom-to-bottom-peak? edge-above highest-peak)
                                 (let*[(index1 (get-field index-first-vertex-in-vertex-vector highest-peak))
                                       (index2 (get-field index-second-vertex-in-vertex-vector highest-peak))
                                       (index3 (get-field index-first-vertex-in-vertex-vector edge-above))
                                       (index4 (get-field index-second-vertex-in-vertex-vector edge-above))
                                       (vertex1-x (get-field x-coor (get-field first-vertex highest-peak)))
                                       (vertex2-x (get-field x-coor (get-field second-vertex highest-peak)))
                                       (vertex3-x (get-field x-coor (get-field first-vertex edge-above)))
                                       (vertex4-x (get-field x-coor (get-field second-vertex edge-above)))
                                       (consed-polygon (if (> vertex1-x vertex4-x)
                                                           (break-vector-into-2 vec index1 index4)
                                                           (break-vector-into-2 vec index2 index3)))
                                       (polygon1 (car consed-polygon))
                                       (polygon2 (cdr consed-polygon))]
                                   (begin ;(display "B")
                                     (set! incomplete #f)
                                     (set! answer (gvector-append
                                                   (send
                                                    (new orthogonal% [size (vector-length polygon1)] [vec polygon1])
                                                    break-to-monotone)
                                                   (send
                                                    (new orthogonal% [size (vector-length polygon2)] [vec polygon2])
                                                    break-to-monotone)))))]))))

                  (for [(i ((λ(x y) (build-list (- y (- x 1)) (λ(j) (+ j x)))) (+ 2 (car possible-iters)) (- (vector-length horizontal-edges) 1)))   ;;;check
                        #:when incomplete]
                    (let* [(edge-below (vector-ref horizontal-edge-sorted-by-y i))
                           (type (send edge-below edge-type))]
                      (if (not (or (equal? type "Top Peak") (equal? type "Top Edge")))
                          (cond [(visible-bottom-to-top-peak? edge-below highest-peak)
                                 (let*[(quad1 (get-field first-vertex highest-peak))
                                       (quad2 (get-field second-vertex highest-peak))
                                       (quad3 (get-field first-vertex edge-below))
                                       (quad4 (get-field second-vertex edge-below))
                                       (index1 (get-field index-first-vertex-in-vertex-vector highest-peak))
                                       (index2 (get-field index-second-vertex-in-vertex-vector highest-peak))
                                       (index3 (get-field index-first-vertex-in-vertex-vector edge-below))
                                       (index4 (get-field index-second-vertex-in-vertex-vector edge-below))
                                       (cons1 (break-vector-into-2 vec index1 index4))
                                       (cons2 (break-vector-into-2 vec index2 index3))
                                       (another-polygon1 (if (or (equal? (vector-ref (car cons1) 1) (vector-ref vec index2))
                                                                 (equal? (vector-ref (car cons1) 1) (vector-ref vec index3)))
                                                             (cdr cons1)
                                                             (car cons1)))
                                       (another-polygon2 (if (or (equal? (vector-ref (car cons2) 1) (vector-ref vec index4))
                                                                 (equal? (vector-ref (car cons2) 1) (vector-ref vec index1)))
                                                             (cdr cons2)
                                                             (car cons2)))
                                       (quadrilateral (vector quad1 quad2 quad3 quad4))
                                       (first-gvector (send
                                                       (make-object orthogonal% (vector-length another-polygon1) another-polygon1)
                                                       break-to-monotone))
                                       (second-gvector (send
                                                        (new orthogonal% [size (vector-length another-polygon2)] [vec another-polygon2])
                                                        break-to-monotone))]
                                   (begin ;(display "A")
                                     (set! incomplete #f)
                                     (set-field! is-special-vertex quad1 #t)
                                     (set-field! is-special-vertex quad2 #t)
                                     (set-field! is-special-vertex quad3 #t)
                                     (set-field! is-special-vertex quad4 #t)   
                                     (gvector-add! first-gvector (new monotone% [size 4] [vec quadrilateral])) 
                                     (set! answer (gvector-append first-gvector second-gvector)))
                                   )])
                          (cond [(visible-top-to-top-peak? edge-below highest-peak)
                                 (let*[(index1 (get-field index-first-vertex-in-vertex-vector highest-peak))
                                       (index2 (get-field index-second-vertex-in-vertex-vector highest-peak))
                                       (index3 (get-field index-first-vertex-in-vertex-vector edge-below))
                                       (index4 (get-field index-second-vertex-in-vertex-vector edge-below))
                                       (vertex1-x (get-field x-coor (get-field first-vertex highest-peak)))
                                       (vertex2-x (get-field x-coor (get-field second-vertex highest-peak)))
                                       (vertex3-x (get-field x-coor (get-field first-vertex edge-below)))
                                       (vertex4-x (get-field x-coor (get-field second-vertex edge-below)))
                                       (consed-polygon (if (> vertex1-x vertex4-x)
                                                           (break-vector-into-2 vec index1 index4)
                                                           (break-vector-into-2 vec index2 index3)))
                                       (polygon1 (car consed-polygon))
                                       (polygon2 (cdr consed-polygon))]
                                   (begin 
                                     (set! incomplete #f)
                                     (set! answer (gvector-append
                                                   (send
                                                    (new orthogonal% [size (vector-length polygon1)] [vec polygon1])
                                                    break-to-monotone)
                                                   (send
                                                    (new orthogonal% [size (vector-length polygon2)] [vec polygon2])
                                                    break-to-monotone)))))]))))) answer))
          (begin (gvector (make-object monotone% size vec)))))
    
    

    (define (visible-bottom-to-top-peak? edge-below highest-peak)
      (visibility 0 1 edge-below highest-peak))
    (define (visible-bottom-to-bottom-peak? edge-above highest-peak)
      (visibility 0 0 edge-above highest-peak))
    (define (visible-top-to-top-peak? edge-below highest-peak)
      (visibility 1 1 edge-below highest-peak))
    (define (visible-top-to-bottom-peak? edge-above highest-peak)
      (visibility 1 0 edge-above highest-peak))

    (define (visibility para1 para2 edge peak)            ;;;;;para means parameter :P
      (define bool-visible #t)
      (define (lie-between val val1 val2)
        (and (< val (max val1 val2)) (> val (min val1 val2))))
      (let [(edgev1x (get-field x-first edge))
            (edgev2x (get-field x-second edge))
            (peakv1x (get-field x-first peak))
            (peakv2x (get-field x-second peak))
            (edgev1y (get-field y-first edge))
            (edgev2y (get-field y-second edge))
            (peakv1y (get-field y-first peak))
            (peakv2y (get-field y-second peak))]
        (cond [(= (modulus (- para1 para2)) 1)              ;;;;;;;;;;;;;;;;;;;;;;top bottom visibility
               (begin
                 (if (< edgev2x peakv1x)
                     (let* [(verticals-in-range (find-within-range vertical-edge-sorted-by-x edgev2x peakv1x))
                            (list-verticals (vector->list verticals-in-range))
                            (slope1 (/ (- peakv2y edgev1y) (- peakv2x edgev1x)))
                            (intercept1 (- peakv2y (* slope1 peakv2x)))
                            (slope2 (/ (- peakv1y edgev2y) (- peakv1x edgev2x)))
                            (intercept2 (- peakv1y (* slope2 peakv1x)))]
                       (for [(i list-verticals)
                             #:when bool-visible]
                         (let* [(x-coors (get-field x-first i))
                                (range-y1 (get-field y-first i))
                                (range-y2 (get-field y-second i))
                                (line1-at-x (+ intercept1 (* slope1 x-coors)))
                                (line2-at-x (+ intercept2 (* slope2 x-coors)))]
                           (cond [(not (and (lie-between line1-at-x range-y1 range-y2)
                                            (lie-between line2-at-x range-y1 range-y2)))
                                  (set! bool-visible #f)]))))
                     (let* [(verticals-in-range (find-within-range vertical-edge-sorted-by-x peakv1x edgev2x))
                            (list-verticals (vector->list verticals-in-range))
                            (slope1 (/ (- peakv2y edgev1y) (- peakv2x edgev1x)))
                            (intercept1 (- peakv2y (* slope1 peakv2x)))
                            (slope2 (/ (- peakv1y edgev2y) (- peakv1x edgev2x)))
                            (intercept2 (- peakv1y (* slope2 peakv1x)))]
                       (for [(i list-verticals)
                             #:when bool-visible]
                         (let* [(x-coors (get-field x-first i))
                                (range-y1 (get-field y-first i))
                                (range-y2 (get-field y-second i))
                                (line1-at-x (+ intercept1 (* slope1 x-coors)))
                                (line2-at-x (+ intercept2 (* slope2 x-coors)))]
                           (cond [(not (and (lie-between line1-at-x range-y1 range-y2)
                                            (lie-between line2-at-x range-y1 range-y2)))
                                  (set! bool-visible #f)])))))   
                 bool-visible)]
              [(= para1 para2)
               (begin
                 (let [(x-sorted-peak-edges (list (cons edgev1x edgev1y) (cons edgev2x edgev2y) (cons peakv1x peakv1y) (cons peakv2x peakv2y)))]
                   (set! x-sorted-peak-edges (sort x-sorted-peak-edges < #:key car))
                   (let* [(cdr-x-sorted-peak-edges (cdr x-sorted-peak-edges))
                          (vertex-1 (car cdr-x-sorted-peak-edges))
                          (vertex-2 (cadr cdr-x-sorted-peak-edges))
                          (v1x (car vertex-1))
                          (v1y (cdr vertex-1))
                          (v2x (car vertex-2))
                          (v2y (cdr vertex-2))
                          (verticals-in-range (find-within-range vertical-edge-sorted-by-x v2x v1x))
                          (list-verticals (vector->list verticals-in-range))
                          (slope (/ (- v2y v1y) (- v2x v1x)))
                          (intercept (- v2y (* slope v2x)))]
                     (for [(i list-verticals)
                           #:when bool-visible]
                       (let* [(x-coors (get-field x-first i))
                              (range-y1 (get-field y-first i))
                              (range-y2 (get-field y-second i))
                              (line-at-x (+ intercept (* slope x-coors)))]
                         (cond [(not (lie-between line-at-x range-y1 range-y2))
                                (set! bool-visible #f)])))))
                 bool-visible)])))))