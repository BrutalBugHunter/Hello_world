#lang racket

(require "edge.rkt")
(require "Polygon.rkt")
(require "coloring.rkt")

(require data/gvector)

(define orthogonal-monotone%
  (class polygon%
    (super-new)
    (inherit-field size vec)
    (field [edge-vec                               
            (vector-append
             (build-vector (- size 1)
                           (λ(i)
                             (new orthogonal-edge%
                                  [first-vertex (vector-ref vec i)]                       
                                  [second-vertex (vector-ref vec (+ i 1))]
                                  [index-first-vertex-in-vertex-vector i]
                                  [index-second-vertex-in-vertex-vector (+ i 1)])))
             (make-vector 1 (new orthogonal-edge%
                                 [first-vertex (vector-ref vec (- size 1))]
                                 [second-vertex (vector-ref vec 0)]
                                 [index-first-vertex-in-vertex-vector (- size 1)]
                                 [index-second-vertex-in-vertex-vector 0])))])
    (field [vertical-edges (make-gvector)])
    (field [horizontal-edges (make-gvector)])

    (define help-edge-index
      (cons
       (car (vector-ref
             (vector-sort
              (build-vector size (λ(i) (let [(the-edge (vector-ref edge-vec i))] (cons i (/ (+ (get-field y-second the-edge) (get-field y-first the-edge)) 2)))))
              > 0 size (λ(x) (cdr x))) 0))
       (car (vector-ref
             (vector-sort
              (build-vector size (λ(i) (let [(the-edge (vector-ref edge-vec i))] (cons i (/ (+ (get-field y-second the-edge) (get-field y-first the-edge)) 2)))))
              > 0 size (λ(x) (cdr x))) 1))))
    
    (define highest-edge-index (car help-edge-index))
    (define lowest-edge-index (cdr help-edge-index))

    (set-field! chain (vector-ref edge-vec highest-edge-index) "both")
    (set-field! chain (vector-ref edge-vec lowest-edge-index) "both")
    
     (if (< highest-edge-index lowest-edge-index)
        (begin
          (for [(i (drop (list-upto lowest-edge-index) (+ highest-edge-index 1)))]
            (set-field! chain (vector-ref edge-vec i) "left"))
          (for [(i (list-upto size))]
            (cond [(equal? (get-field chain (vector-ref edge-vec i)) #f)
                   (set-field! chain (vector-ref edge-vec i) "right")])))
        (begin
          (for [(i (drop (list-upto highest-edge-index) (+ lowest-edge-index 1)))]
            (set-field! chain (vector-ref edge-vec i) "right"))
          (for [(i (list-upto size))]
            (cond [(equal? (get-field chain (vector-ref edge-vec i)) #f)
                   (set-field! chain (vector-ref edge-vec i) "left")]))))
    
    
        
          
    (define (horizontal-vertical-edges)
      (define list-to-iterate (append (drop (list-upto size) highest-edge-index) (list-upto highest-edge-index)))
      (define y (modulo highest-edge-index 2))
      (define x 0)
      (for [(i list-to-iterate)]
        (cond [(= x y) (gvector-add! horizontal-edges (vector-ref edge-vec i))]
              [#t (gvector-add! vertical-edges (vector-ref edge-vec i))])
        (set! x (+ x 1))))
    (horizontal-vertical-edges)

    (define h-size (gvector-count horizontal-edges))
    
    (define sorted-horizontal-edges
      (vector-map
       (λ(x) (car x))
       (vector-sort
        (build-vector h-size (λ(i) (let [(the-edge (vector-ref edge-vec i))] (cons the-edge (/ (+ (get-field y-second the-edge) (get-field y-first the-edge)) 2)))))
        > 0 size (λ(x) (cdr x))) 0))

    (define STACK (cons (vector-ref sorted-horizontal-edges 0) '())) 

    (define (handler edge)
      (cond [(equal? (send edge edge-type) "Top Edge") (begin (set! STACK (cons edge STACK)) (make-vector 0))]
            [(equal? (get-field chain (car STACK)) "both") (let* [(new-edge #f)
                                                                  (top-edge-current (car STACK))
                                                                  (quadrilateral (vector (make-object node% 4 (vector (get-field first-vertex edge)
                                                                                                                      (get-field second-vertex edge)
                                                                                                                      (get-field first-vertex top-edge-current)
                                                                                                                      (get-field second-vertex top-edge-current)))))]
                                                             (if (equal? (get-field chain edge) "left")
                                                                 (set! new-edge (make-object edge% (get-field first-vertex top-edge-current) (get-field second-vertex edge) #f #f))
                                                                 (set! new-edge (make-object edge% (get-field first-vertex top-edge-current) (get-field second-vertex edge) #f #f)))
                                                             (set! STACK new-edge)
                                                             quadrilateral)]
            [(equal? (get-field chain edge) (get-field chain (car STACK)))
               (let* [(top-edge-current (car STACK))
                      (quadrilateral (vector (make-object node% 4 (vector (get-field first-vertex edge)
                                                                          (get-field second-vertex edge)
                                                                          (get-field first-vertex top-edge-current)
                                                                          (get-field second-vertex top-edge-current)))))
                      (new-edge #f)]
                 (set! STACK (cdr STACK))
                 (if (equal? (get-field chain edge) "left")
                     (set! new-edge (make-object orthogonal-edge%  (get-field first-vertex top-edge-current) (get-field second-vertex edge) #f #f))
                     (set! new-edge (make-object orthogonal-edge%  (get-field first-vertex edge) (get-field second-vertex top-edge-current) #f #f)))
                 (set-field! is-special-vertex (get-field first-vertex new-edge) #t)
                 (set-field! is-special-vertex (get-field second-vertex new-edge) #t)
                 (cond [(or (equal? (send new-edge edge-type) "Bottom Edge") (equal? (send new-edge edge-type) "Top Edge"))
                        (vector-append quadrilateral (handler new-edge))]
                       [#t
                        (begin
                          (send new-edge set-code-for-type-manually 3)
                          (vector-append (handler new-edge) (vector quadrilateral)))]))]
            [#t
             (let* [(top-edge-current (car STACK))
                    (quadrilateral (vector (make-object node% 4 (vector (get-field first-vertex edge)
                                                                        (get-field second-vertex edge)
                                                                        (get-field first-vertex top-edge-current)
                                                                        (get-field second-vertex top-edge-current)))))
                    (new-edge1 #f)
                    (new-edge2 #f)]
               (if (equal? (get-field chain edge) "left")
                   (begin
                     (set! new-edge1 (make-object orthogonal-edge%  (get-field first-vertex edge) (get-field second-vertex top-edge-current)))
                     (set! new-edge2 (make-object orthogonal-edge%  (get-field first-vertex top-edge-current) (get-field second-vertex edge))))           
                   (begin
                     (set! new-edge1 (make-object orthogonal-edge%  (get-field first-vertex top-edge-current) (get-field second-vertex edge)))
                     (set! new-edge2 (make-object orthogonal-edge%  (get-field first-vertex edge) (get-field second-vertex top-edge-current)))))
               (set! STACK (cons new-edge1 STACK))
               (let* [(pyramid-vector (pyramidizing-and-quad STACK))]
                 (set! STACK (cons new-edge2 STACK))
                 (vector-append pyramid-vector quadrilateral)))]))

    
    (define (pyramidizing-and-quad STACK)   ;;STack is a vector of edges 
      (define (edge-to-vertices edge)
        (let* [(vertex1 (get-field first-vertex edge))
               (vertex2 (get-field second-vertex edge))]
          (if (> (get-field y-coor vertex1) (get-field y-coor vertex2)) (vector vertex1 vertex2)
              (vector vertex2 vertex1))))
      (quadrangulate2 (vector-append* (vector-map (lambda (x) (edge-to-vertices x)) STACK))))
               
    
    
                 
    (define (construct)
      (define l (make-vector 0))
      (for [(i (cdr (vector->list sorted-horizontal-edges)))]
        (set! l (vector-append (handler i) l))))))
    
    