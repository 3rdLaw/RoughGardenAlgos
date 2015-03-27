#lang racket
(require graph data/heap)

;;generic comparator
(define (compare frst scnd)
  (define proc (match frst
                 [(? symbol?)    symbol<?]
                 [(? number?)    <=]
                 [(? string?)    string<=?]))
  (frst . proc . scnd))


;;O(m)
(define (get-edge-weights graph)
  (define edges (remove-duplicates (map (curryr sort compare) 
                                        (get-edges graph))))
  (map (λ (lst) (match-define (list x y) lst)
         `(,x ,y ,(edge-weight graph x y)))
       edges))

;;O(n*m)
(define (prim-naive graph)
  (define VERTICES (get-vertices graph))
  
  
  ;;O(m), but dropped with loop below
  (define EDGE-WEIGHTS (get-edge-weights graph))
  
  (define (pick-random-vertex)
    (list-ref VERTICES (random (length VERTICES))))
  
  (define (unspanned? nodes x) 
    (match-define (list start end num) x)
    ;if either start or end but not both are members of nodes: aka xor
    (xor (member start nodes) (member end nodes)))
  
  ;;repeated n times, so O(n*m)
  (let loop ([nodes `(,(pick-random-vertex))]  [tree '()])
    (cond [(equal? (length nodes) (length VERTICES)) tree];if all nodes reached, return tree
          [else (let* ([available-edges (filter (curry unspanned? nodes) EDGE-WEIGHTS)];O(m)
                       [min-edge (argmin third available-edges)]);O(m)
                  (loop (cons (first (remove* nodes min-edge))
                              nodes)
                        (cons min-edge tree)))])))

;;O(m*log(n))
(define (prim-heap-fancy graph)
  (define distance-hash (make-hash))
  ;heap, with comparator that references distance-hash (above)
  (define heap (make-heap (λ (x y) ((hash-ref distance-hash x (λ () +inf.0)) . <= .
                                    (hash-ref distance-hash y (λ () +inf.0))))))

  ;find the edge which is being used to move the frontier
  (define (find-edge nodes cur-node dist)
    (for/or ([x (in-list nodes)])
      (and (= (edge-weight graph x cur-node) dist);if exists and weight is correct
           `(,x ,cur-node))));return it
  
  (define VERTICES (get-vertices graph))

  (define starting-vertex (list-ref VERTICES (random (length VERTICES))))

  ;;O(n-1) - add all edges aside from starting into heap, but dropped with mult. below
  (for ([node (in-list (remove starting-vertex VERTICES))])
    ;edge-weight returns +.inf if edge doesn't exist
    (hash-set! distance-hash node (edge-weight graph starting-vertex node))
    (heap-add! heap node))

  ;;Overall O(m*log(n))
  (let loop ([nodes `(,starting-vertex)]  [tree '()])
    (cond [(equal? (length nodes) (length VERTICES)) tree]
          [else (define cur-node (heap-min heap))
                (heap-remove-min! heap)
                (define neighbors (filter (λ (x) (not (member x nodes)))
                                          (get-neighbors graph cur-node)))

                ;;for each neighbor we need to possibly update value, but O(m) max overall
                (for ([x (in-list neighbors)])
                  (heap-remove! heap x);remove
                  (hash-set! distance-hash x (min (hash-ref distance-hash x);old val
                                                  (edge-weight graph cur-node x)));new val?
                  (heap-add! heap x));re-add
                
                ;;O(m) but constant in practice, to find out exactly which edge we're adding
                ;;if we did a better job with datastructures we could probably eliminate this.
                (define the-edge (find-edge nodes cur-node (hash-ref distance-hash cur-node)))
                
                (loop (cons cur-node nodes)
                      (cons `(,@the-edge ,(edge-weight graph (car the-edge) cur-node)) tree))])))


(module+ test
  (require rackunit)

  ;;Swap weight from 3rd to 1st value in list
  (define (convert-num-third-to-first lst)
    (map (λ(x) (match-define (list start end weight) x)
           `(,weight ,start ,end))
         lst))

  ;;Lexigraphically order trees
  (define (standardize lst)
    (sort 
     (map (λ(x) (match-define (list start end weight) x)
            (if (compare start end) ;put lexigraphically-smaller node first
                `(,start ,end ,weight)
                `(,end ,start ,weight)))
          lst)
     compare
     #:key car))

  (define (same? g)
    (let* ([one (prim-naive g)]
           [two (prim-heap-fancy g)])
      ;if sums are equal (within rounding error of 0.0001 produced by heap datastructure)
      (if (   (abs ((apply + (map third one)) . - . (apply + (map third two)))) . < .  0.0001)
          #t ;return true and bail
          (begin ;otherwise print out the differences
            (printf "First ~a\nchain ~a\nlength: ~a\n\n"  one 
                    (map third one)
                    (apply + (map third one)))
            (printf "Second ~a\nchain ~a\nlength: ~a\n\n" two
                    (map third two)
                    (apply + (map third two)))
            #f))))

  (define g (weighted-graph/undirected '((5 a b) (2 b d) (1 a d) (3 c d))))
  (check-equal? (same? g) #t)

  (define g1
    (weighted-graph/undirected
     (convert-num-third-to-first
      '((1 2 10)
        (1 5 -3)
        (1 4 5)
        (1 3 4)
        (2 6 6)
        (2 3 7)
        (3 6 -10)
        (3 4 -1)
        (4 6 2)
        (4 5 -8)
        (5 6 1)))))
  (check-equal? (same? g1) #t)

  (define g2
    (weighted-graph/undirected
     (convert-num-third-to-first
      '((1 2 1)
        (1 4 4)
        (2 3 2)
        (2 4 1)
        (3 4 3)))))

  (check-equal? (same? g2) #t)

  (define g3
    (weighted-graph/undirected
     (convert-num-third-to-first
      '((4 5 0.35)
        (4 7 0.37)
        (5 7 0.28)
        (0 7 0.16)
        (1 5 0.32)
        (0 4 0.38)
        (2 3 0.17)
        (1 7 0.19)
        (0 2 0.26)
        (1 2 0.36)
        (1 3 0.29)
        (2 7 0.34)
        (6 2 0.40)
        (3 6 0.52)
        (6 0 0.58)
        (6 4 0.93)))))
  (check-equal? (same? g3) #t))
