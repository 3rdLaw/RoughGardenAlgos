#lang racket
(require graph data/heap)

;; O( (|E|+|V|) * log(|V|) )
(define (my-dijkstra g v)
  ;hash where vertex ->  min distance to that vertex
  (define distance-hash (make-hash))
  ;heap, with comparator that references distance-hash (above)
  (define heap (make-heap (λ (x y) ((hash-ref distance-hash x (λ () +inf.0)) . <= .
                                    (hash-ref distance-hash y (λ () +inf.0))))))

  ;|V|*log(|V|), but this gets dropped with the V multiplication below
  (for ([x (in-list (get-vertices g))])
    (if (equal? x v);if start
        (hash-set! distance-hash x 0);set initial distance of 0
        (hash-set! distance-hash x +inf.0));set initial distance
    (heap-add! heap x));add to heap (with initial distance)

  ;;a couple possible log(|V|) calls, depending
  (define relax
    (match-lambda**
     [((list from to) path)
      ;calculated distance to this neighbor given our current path
      (define new-dist (+ (hash-ref distance-hash from) (edge-weight g from to)))
      ;is this node the fastest way to get to a next node?
      (cond [(new-dist . < . (hash-ref distance-hash to));if so
             (hash-set! distance-hash to new-dist);update distance
             (heap-remove! heap to);remove b/c old distance
             (heap-add! heap to);re-add to heap with shorter distance
             (hash-set path to from)];update & return path
            ;else keep path & distance unchanged
            [else path])]))

  ;;inner loop is (|V| + |E|) * times some number of heap operations (which are all log(|V|))
  (let loop ([path (hash)]);we loop over each vertex through our heap, thus |V|
    (if  ((heap-count heap) . = . 0);if no more vertices
         (values distance-hash path);return distances & paths
         (let ([cur-node (heap-min heap)]);grab closest vertex to current boundary
           (heap-remove-min! heap);all heap operations are log(b)
           ;each edge can appear only once in a new-path, so |E|
           (define new-path (foldl relax
                                   path
                                   (map (λ (nd) `(,cur-node ,nd));put into pair form
                                        ;constant time (adjacency-list is a hash)
                                        (get-neighbors g cur-node))))
           (loop new-path)))))

(define g (weighted-graph/directed '((14 a b)
                                     (9 a c)
                                     (7 a d)
                                     (9 b e)
                                     (2 b c)
                                     (10 b d)
                                     (6 d f)
                                     (11 c f)
                                     (15 e f))))

(my-dijkstra g 'a)

(module+ test
  (require rackunit)

  (define (test-graph g)
    ;;standardizes the output between mine & system's dijkstra
    (define (clean-up_ hsh start)
      (sort (filter-not (λ (x) (match-define (cons strt end) x)
                           (eqv? end #f))
                        (hash->list hsh))
            (λ (x y)  ( (car x) . symbol<? . (car y)))))

    (for ([x (in-vertices g)]);run dijkstra on every node
      (let*-values ([(start) x]
                    [(clean-up) (curryr clean-up_ start)]
                    [(my-dist my-path) (my-dijkstra g start)]
                    [(their-dist their-path) (dijkstra g start)])
        (check-equal? my-dist their-dist)
        (check-equal? (clean-up my-path) (clean-up their-path)))))


  (test-graph (weighted-graph/directed '((1 a b)
                                         (3 a c)
                                         (2 b c)
                                         (4 c d)
                                         (6 b d))))

  (test-graph (weighted-graph/directed '((1 a b)
                                         (2 a c)
                                         (2 c d)
                                         (3 b d)
                                         (4 b e)
                                         (5 c f)
                                         (2 f h)
                                         (3 h j)
                                         (8 j z)))))
