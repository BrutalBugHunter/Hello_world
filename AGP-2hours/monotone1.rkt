#lang racket

(require "vertex.rkt")
(require "edge.rkt")
(require "Polygon.rkt")
(require "coloring.rkt")
(require data/gvector)
(provide (all-defined-out))
#|
1.i need a function to form vector of nodes into a tree of gnodes linear tree (where should i put this function to maintain modularity?)
2.i need to know the vertices are in left branch or right branch then i need to form the pair of it with vertex and sort the pair by its y ordinate
3.break-monotone-to-node would be the name of the abstract function which utlixes the intrinsic property of monotone polygons and a function which is passed to it as arguement which defines how
 monotone plygon should be partitioned does the job
4.i first return the list of nodes then i form a tree of them using the construct gnode
5.i also need to decide whether a vertex is in right branch or left branch|#
(provide (all-defined-out))


(define monotone%
  (class polygon%
    (super-new)
    (inherit-field size vec)
    ; (init-field edge-vec)
    
    (define (dot-product vec1 vec2)                   ;;; subroutine for check-diagonal
      (let* [(point1 (car vec1))
             (point2 (cadr vec1))
             (point3 (car vec2))
             (point4 (cadr vec2))
             (diff-x1 (- (car point2) (car point1)))
             (diff-y1 (- (cadr point2) (cadr point1)))
             (diff-x2 (- (car point4) (car point3)))
             (diff-y2 (- (cadr point4) (cadr point3)))]
        (+ (* diff-x1 diff-x2) (* diff-y1 diff-y2))))



    (define (rotate-left vec)                          ;;; subroutine for check-diagonal
      (let* [(point1 (car vec))
             (point2 (cadr vec))
             (x1 (car point1))
             (y1 (cadr point1))
             (x2 (car point2))
             (y2 (cadr point2))]
        (list (list y2 x1) (list y1 x2))))
    

    (define (neighbours vertex)   
      (define (helper neighbouring-vertices)
        (if (equal? (cadr (car neighbouring-vertices)) vertex) (car neighbouring-vertices) (helper (cdr neighbouring-vertices))))
      (helper (triplet-list vec)))                 ;; triplet-vec returns a list

    (define (check-diagonal vertex1 vertex2)       
      (let* [(vertex-prev1 (car (neighbours vertex1)))                    ;;; i will define vector as a pair of pairs and a function which rotates the vectors to left previous refers to earlier vertice 
             (vertex-next1 (caddr (neighbours vertex1)))                   ;;; in counterclockwise order we 
             (vertex-prev2 (car (neighbours vertex2)))
             (vertex-next2 (caddr (neighbours vertex2)))
;             (xd1 (get-field x-coor vertex1))
;             (yd1 (get-field y-coor vertex1))
;             (xd2 (get-field x-coor vertex2))
;             (yd2 (get-field y-coor vertex2))
;             (xp1 (get-field x-coor vertex-prev1))
;             (yp1 (get-field y-coor vertex-prev1))
;             (xn1 (get-field x-coor vertex-next1))
;             (yn1 (get-field y-coor vertex-next1))
;             (xp2 (get-field x-coor vertex-prev2))
;             (yp2 (get-field y-coor vertex-prev2))
;             (xn2 (get-field x-coor vertex-next2))
;             (yn2 (get-field y-coor vertex-next2))
;             (diagonal-from1 (list (list xd1 yd1) (list xd2 yd2)))
;             (diagonal-from2 (list (list xd2 yd2) (list xd1 yd1)))
;             (in-edge-prev1 (rotate-left (list (list xp1 yp1) (list xd1 yd1))))
;             (in-edge-next1 (rotate-left (list (list xd1 yd1) (list xn1 yn1))))
;             (in-edge-prev2 (rotate-left (list (list xp2 yp2) (list xd2 yd2))))
;             (in-edge-next2 (rotate-left (list (list xd2 yd2) (list xn2 yn2))))
;             (dot-prod1 (dot-product diagonal-from1 in-edge-prev1))
;             (dot-prod2 (dot-product diagonal-from1 in-edge-next1))
;             (dot-prod3 (dot-product diagonal-from2 in-edge-prev2))
;             (dot-prod4 (dot-product diagonal-from2 in-edge-next2))
             (thetha1 (angle vertex-prev1 vertex1 vertex2))
             (thetha2 (angle vertex2 vertex1 vertex-next1))
             (thetha3 (angle vertex-prev1 vertex1 vertex-next1))
             (thetha4 (angle vertex-prev2 vertex2 vertex1))
             (thetha5 (angle vertex1 vertex2 vertex-next2))
             (thetha6 (angle vertex-prev2 vertex2 vertex-next2))]
        (if (and (= thetha3 (+ thetha1 thetha2)) (= thetha4 (+ thetha5 thetha6))) #t #f)))

    (define/public (angle point1 point point2)                                   ;;;;;;;ASSUMING ANTICLOCKWISE ORDER IN POINT1-POINT-POINT2
      (let* [(x1 (send point1 vertex-xcoor))
             (x2 (send point2 vertex-xcoor))
             (y1 (send point1 vertex-ycoor))
             (y2 (send point2 vertex-ycoor))
             (x-coor (send point vertex-xcoor))
             (y-coor (send point vertex-ycoor))
             (vector-1 (cons (- x-coor x1) (- y-coor y1)))
             (vector-2 (cons (- x-coor x2) (- y-coor y2)))
             (dot-product (+ (* (car vector-1) (car vector-2)) (* (cdr vector-1) (cdr vector-2))))
             (cross-product (- (* (car vector-1) (cdr vector-2)) (* (car vector-2) (cdr vector-1))))
             (mag-v1 (sqrt (+ (* (car vector-1) (car vector-1)) (* (cdr vector-1) (cdr vector-1)))))
             (mag-v2 (sqrt (+ (* (car vector-2) (car vector-2)) (* (cdr vector-2) (cdr vector-2)))))
             (some-angle (* 180 (/ 1 pi) (acos (/ dot-product (* mag-v1 mag-v2)))))
             (unit-vector-1 (cons (/ (- x-coor x1) mag-v1) (/ (- y-coor y1) mag-v1)))
             (unit-vector-2 (cons (/ (- x-coor x2) mag-v2) (/ (- y-coor y2) mag-v2)))
             (angle-bisector (cons (+ (/ (- x1 x-coor) mag-v1) (/ (- x2 x-coor) mag-v2)) (+ (/ (- y1 y-coor) mag-v1) (/ (- y2 y-coor) mag-v2))))]
        (if (> cross-product 0) (- 360 some-angle) some-angle)))
;            (begin
;              (set! angle-at-vertex (- 360 some-angle))
;              (cond                ;;;;;;;;;;;;reflex angle case
;                [(and (> y-coor y1) (> y-coor y2))
;                 (begin (set! code-for-type 2) (- 360 some-angle))]
;                [(and (< y-coor y1) (< y-coor y2))
;               (begin (set! code-for-type 3) (- 360 some-angle))]
;                ;[#t (- 360 some-angle)])
;                [(> (car angle-bisector) 0)
;                 (begin (set! code-for-type 5) (- 360 some-angle))]
;                [#t
;                 (begin (set! code-for-type 6) (- 360 some-angle))]))
;            (begin
;              (set! angle-at-vertex some-angle)
;              (cond                ;;;;;;;;;;;;;;angle is not reflex
;                [(and (> y-coor y1) (> y-coor y2))
;                 (begin (set! code-for-type 1) some-angle)]
;                [(and (< y-coor y1) (< y-coor y2))
;               (begin (set! code-for-type 4) some-angle)]
;                [(> (car angle-bisector) 0)
;                 (begin (set! code-for-type 6) some-angle)]
;                [#t
;                 (begin (set! code-for-type 5) some-angle)])))))
      



    (define (rotate-list lst)
      (append (cdr lst) (list (car lst))))
    (define (process-input1 lst)              ;process-imput1 accepts list
      (define (helper lst optimal-pair)
        (if (equal? optimal-pair (car lst)) lst (helper (rotate-list lst) optimal-pair)))
      (let* [(optimal-pair  (foldr (lambda (x y) (cond [(> (get-field y-coor x) (get-field y-coor y)) x]
                                                       [(= (get-field y-coor x) (get-field y-coor y)) (if (< (get-field x-coor x) (get-field x-coor y)) x y)]
                                                       [#t y]))  (car lst) lst))]
        (helper lst optimal-pair)))
    (define the-final-list-for-triangulater (process-input1 (vector->list vec)))
    (define/public (triangulate-it) (vector-map (λ(x) (mcons (mcar x) (vector->gvector (mcdr x))))
                                                (make-typical-data-structure (triangulater the-final-list-for-triangulater)))) 
    (define/public (triangulater final-list-of-vertices)                   ;;final list of vertices is the processed vertex
      (if (= 3 (length final-list-of-vertices))
          (make-triangle final-list-of-vertices)              ;;make-triangle makes nodes
          (let*[(pair-triplet1 (pick-three-vertices 'left-tringle   final-list-of-vertices))      ;;X-triangle function returns a pair of vector of three vertices in y order and vertex to be exculded otherwise #f
                (pair-triplet2 (pick-three-vertices 'right-tringle  final-list-of-vertices))      ;;we define a trim polygon function
                (pair-triplet3 (pick-three-vertices 'middle-tringle final-list-of-vertices))
                (valid-triangle (The-chosen-one pair-triplet3 pair-triplet2 pair-triplet1))                                                                    ;;
                (The-node (make-node valid-triangle))
                (remaining-polygon (trim-polygon valid-triangle final-list-of-vertices))
                (vector-of-triangles (triangulater remaining-polygon))]
            (vector-append The-node vector-of-triangles))
          ))
              
    (define (The-chosen-one triangle1 triangle2 triangle3)              ;;valid? checks whether the third edge is in the polygon triangle is the pair triplet
      (cond [(valid? triangle1) triangle1]
            [(valid? triangle2) triangle2]
            [#t triangle3]))         
     
    (define (valid? triangle)
      (let*[(lonely-edge (cdr triangle))
            (list-of-three-vertices (car triangle))
            (two-vertices-of-diagonals (remove lonely-edge list-of-three-vertices))]
        (check-diagonal (car two-vertices-of-diagonals) (cadr two-vertices-of-diagonals))))
             

    (define (pick-three-vertices instruction collection)              ;;returns pair of vector of three vertices in y order and vertex to be exculded or #f if not possible instruction is teh sym
      (cond [(eq?  instruction 'left-tringle)
             (let* [(start-vertex (car collection))
                    (next-two-vertex (two-vertex 'left (cdr collection)))   ;; its a list of two vertexes in order counterclockwise               
                    (three-vertices (cons start-vertex next-two-vertex))]
               (cons three-vertices (cadr three-vertices)))]
            [(eq?  instruction 'right-tringle)
             (let* [(start-vertex (car collection))
                    (next-two-vertex (two-vertex 'right (cdr collection)))   ;; its a list of two vertexes in order counterclockwise             
                    (three-vertices (cons start-vertex next-two-vertex))]
               (cons three-vertices (cadr three-vertices)))]
            [(eq?  instruction 'middle-tringle)
             (let* [(start-vertex (car collection))
                    (next-two-vertex (two-vertex 'left-right (cdr collection)))   ;; its a pair of two vertexes in order counterclockwise           
                    (three-vertices (cons start-vertex next-two-vertex))]
               (cons three-vertices (car three-vertices)))]))
             
                     
    (define (filter p l)
      (if (p (car l)) (cons (car l) (filter p (cdr l))) (filter p (cdr l)))) 


    (define (two-vertex instruction collection)         ;; collection is the final list of vertexes                           ;; returns two-vertices or false if two vertices cant be selected.
      (cond [(eq? instruction 'left) (list (car collection) (cadr collection))]
            [(eq? instruction 'right)
             (let [(collection1 (reverse collection))] (list (car collection1) (cadr collection1)))] 
            [(eq? instruction 'left-right) (list (car collection) (car (reverse collection)))]))
              


    (define (trim-polygon triangle polygon)                                ;;trim-polygon returns another final-list-of-vertexs with topmost at start and same properties by removing triangle
      (let [(vertex1 (cdr triangle))]
        (process-input1 (remove vertex1 polygon))))      

    (define (make-triangle list-of-vertices)                                ;; here the list isnt consed 
      (let* [(ordered-list-of-three-vertices (make-counterclockwise list-of-vertices))]
        (make-vector 1 (make-object node% 3 (list->vector ordered-list-of-three-vertices)))))  
              
    ;the base case returns the vector of the node                            
    (define (make-node triangle)                                          ;;triangle is basically the triplet its not consed dont forget that returns the vector of the node                         
      (let* [(list-of-three-vertices (car triangle))
             (ordered-list-of-three-vertices (make-counterclockwise list-of-three-vertices))]
        (make-vector 1 (make-object node% 3 (list->vector ordered-list-of-three-vertices)))))

    (define (make-counterclockwise list-of-three-vertices)
      (if (determinant list-of-three-vertices) list-of-three-vertices
          (let* [(first (car list-of-three-vertices))
                 (second(cadr list-of-three-vertices))
                 (third (caddr list-of-three-vertices))]
            (list first third second))))

    (define (determinant list-of-three-vertices)
      (let* [(first (car list-of-three-vertices))
             (second(cadr list-of-three-vertices))
             (third (caddr list-of-three-vertices))
             (x1 (get-field x-coor first))
             (y1 (get-field y-coor first))
             (x2 (get-field x-coor second))
             (y2 (get-field y-coor second))
             (x3 (get-field x-coor third))
             (y3 (get-field y-coor third))]
        (> 0 (- (* (- x1 x2) (- y2 y3)) (* (- x2 x3) (- y1 y2))))))

    (define (triplet-list vec)     ;; vec is the vector of vertices it gives a list of triplets 
      (define (make-triplets l1 l2 l3)
        (if (null? l1) '()
            (cons (list (car l1) (car l2) (car l3)) (make-triplets (cdr l1) (cdr l2) (cdr l3))))) 
      (let* [(list-vertices (vector->list vec))
             (rotate-left1 (rotate-list list-vertices))
             (rotate-right1 (reverse (rotate-list (reverse list-vertices))))]
        (make-triplets rotate-right1 list-vertices rotate-left1)))

    (define (make-typical-data-structure vec-of-nodes)     ;;l is the vector of nodes i return a list of pairs of nodes consed with vector of adjacent nodes  
      (if (= (vector-length vec-of-nodes) 1)
          (vector (mcons (vector-ref vec-of-nodes 0) (make-vector 0))) 
          (let* [(start (mcons (vector-ref vec-of-nodes 0) (vector (vector-ref vec-of-nodes 1))))
                 (end   (mcons (vector-ref vec-of-nodes (- (vector-length vec-of-nodes) 1)) (vector (vector-ref vec-of-nodes (- (vector-length vec-of-nodes) 2)))))
                 (vec1  (vector-drop-right vec-of-nodes 2))
                 (vec2  (vector-drop vec-of-nodes 2))
                 (vec3  (vector-drop-right (vector-drop vec-of-nodes 1) 1))
                 (vec4  (build-combined-vec vec1 vec2))
                 (vec5  (build-data-structure vec3 vec4))]
            (vector-append (vector start) (list->vector vec5) (vector end)))))
    
    (define (build-combined-vec vec1 vec2)
      (if (= 0 (vector-length vec1)) (make-vector 0)
            (vector-append (vector (vector (vector-ref vec1 0) (vector-ref vec2 0))) (build-combined-vec (vector-drop vec1 1) (vector-drop vec2 1)))))
    
    (define (build-data-structure vec4 vec5)
      (if (= 0 (vector-length vec4)) '()
          (append (list (mcons (vector-ref vec4 0) (vector-ref vec5 0))) (build-data-structure (vector-drop vec4 1) (vector-drop vec5 1)))))
    ))


  
  