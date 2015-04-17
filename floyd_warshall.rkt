#lang racket

(require graph)

;return only results from round val:
(define (filter-hash hsh val)
  (for*/hash ([(k v) (in-hash hsh)]
              [start (in-value (first k))]
              [end (in-value (second k))]
              [nodes (in-value (third k))]
              #:when (= nodes val))
    (values `(,start ,end) v)))

;;O(V^3)
(define (my-floyd-warshall graph [debug #f])
  (define nodes (get-vertices graph))
  (define MAX-NODE-ID (length nodes))

  (define hsh (make-hash))
  (define (hsh-set! start end nodes value)
    (hash-set! hsh `(,start ,end ,nodes) value))
  (define (hsh-get start end nodes)
    (hash-ref hsh `(,start ,end ,nodes) #;(λ () +inf.0)
              (λ () (error (format "Unexpected hsh-get with ~a ~a ~a" start end nodes)))))
  ;O(V^2)
  (for* ([i (in-list nodes)]
         [j (in-list nodes)])
    (cond [(equal? i j)
           (hsh-set! i j 0 0)]
          [else ;this returns the edge weight, or +inf.0 if no edge
           (hsh-set! i j 0 (edge-weight graph i j))]))
  ;O(V^3)
  (for* ([k (in-range 1 (add1 MAX-NODE-ID))]
         [i (in-list nodes)]
         [j (in-list nodes)])
    (hsh-set! i j k (min (hsh-get i j (sub1 k))
                         (+ (hsh-get i k (sub1 k))
                            (hsh-get k j (sub1 k))))))

  
  (if (for/or ([i (in-list nodes)])  ;detect negative cycle by a negative value for a node to itself
        ((hsh-get i i MAX-NODE-ID) . < . 0))
      (begin (when debug (displayln "Negative Cycle found!") (filter-hash hsh MAX-NODE-ID))
             #f)
      (filter-hash hsh MAX-NODE-ID)))

(my-floyd-warshall (weighted-graph/directed '((5 1 3) (10 1 2) (20 2 3))))
(my-floyd-warshall (weighted-graph/directed '((2 1 3) (4 1 5) (1 3 5) (2 3 4) (2 4 2) (4 5 2))))

(module+ test
  (require rackunit)

  (define (hash-equivalent? a b)
    (for/and ([(k v) (in-hash a)])
      (and (hash-has-key? b k)
           ;if both are infinity, we're good
           (if (and (= v +inf.0) (= v (hash-ref b k)))
               #t ;otherwise abs the diff to deal w/ rounding errors
               ((abs (- (hash-ref b k) v)) . < . 0.001)))))

  (define (check-hash-equivalent? a b)
    (check-true (hash-equivalent? a b)
                (~a "Hashes: " a " and " b " are not equivalent!")))

  (for* ([lst (in-list '(((5 1 3) (10 1 2) (20 2 3))
                         ((2 1 3) (4 1 5) (1 3 5) (2 3 4) (2 4 2) (4 5 2))
                         ((5 1 2) (5 1 3) (5 1 4) (5 1 5))
                         ((1 1 2) (2 1 3) (2 3 4) (3 2 4) (4 2 5) (5 3 6)
                          (2 6 7) (3 7 8) (8 8 9))))]
         [g (in-value (weighted-graph/directed lst))])
    (check-hash-equivalent? (my-floyd-warshall g)
                            (floyd-warshall g))))
