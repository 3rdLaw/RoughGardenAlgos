#lang racket
(require graph)

(provide (contract-out [my-bellman-ford (-> graph? any/c (or/c (listof hash?) #f))]))

;remove all but results from the val round, and strip out round numbers
(define (filter-hash hsh val)
  (for*/hash ([(k v) (in-hash hsh)]
              [len (in-value (first k))]
              [node (in-value (second k))]
              #:when (= len val))
    (values node v)))

(define (hash-min-value hsh)
  (let ([vals (hash-values hsh)])
    (if (empty? vals)
        +inf.0
        (apply min vals))))

;;; Fast, ugly, iterative (non-functional) version
;; assumes vertices are numbers
(define (my-bellman-ford graph start [debug #f])
  (let/cc bail
    (define nodes (get-vertices graph))
    (define MAX-PATH-LENGTH (length nodes))
    (define edges (get-edges graph))
    
    ;; We calculate & store the in-edges in a closure to avoid repeating it
    ;;  this makes a difference for big graphs
    ;; O(V*E)
    (define get-in-edges
      (let* ([get-incoming-edges
              (lambda (x)
                (values x (filter-map
                           (match-lambda [(list a b)
                                          (and (equal? b x) a)])
                           edges)))]
             [in-edges-hsh (for/hash ([x (in-list nodes)])
                             (get-incoming-edges x))])
        (lambda (node)
          (hash-ref in-edges-hsh node (λ () '())))))

    ;hash stores distances
    (define hsh (make-hash))
    (define (hsh-set! len node value)
      (hash-set! hsh `(,len ,node) value))
    
    (define (hsh-get len node)
      (hash-ref hsh `(,len ,node) (λ () +inf.0)))
    
    ;zero-length path to start from start
    (hsh-set! 0 start 0)

    ;holds best neighbor
    (define path-hsh (make-hash))

    ;; Main loop
    ;;n^2 subproblem, max n work per subproblem, so high bound n^3 for dense graph,
    ; but total work is O(n*sum(in-degree(v), For All v)), and sum(in-degree(v), For All v)=m
    ; so tighter bound is O(V*E)
    (for* ([i (in-range MAX-PATH-LENGTH)]
           [v (in-list nodes)])
      ;using i-1 edges
      (define previous-best (hsh-get (if (= i 0) 0 (sub1 i))
                                     v))
      ;using i edges allotment, get incoming edges
      (define full-length-options (get-in-edges v))
      ; hash w/ node -> distance for above
      (define full-length-options-weights
        (for/hash ([w (in-list full-length-options)])
          (values w (+ (hsh-get (sub1 i) w) (edge-weight graph w v)))))

      (if (previous-best . <= . (hash-min-value full-length-options-weights))
          (hsh-set! i v previous-best) ;no change to best
          (begin
            (hsh-set! i v (hash-min-value full-length-options-weights))
            (hash-set! path-hsh v (argmin (λ (x) (hash-ref full-length-options-weights x))
                                          full-length-options))))
      ;stopping early
      (when (and (equal? v (last nodes)) ;if we're at the end of a full 'v' cycle iteration
                 (not (= i (sub1 MAX-PATH-LENGTH))) ;and we're not completely done,
                 (for/and ([node (in-list nodes)]) ;and no change to the best values between
                   (= (hsh-get (sub1 i) node) ;both the past and current rounds
                      (hsh-get i node))))
        ;then we can bail
        (begin (when debug (displayln (~a "Stopped early! Final level is " i ".")))
               (bail `(,(filter-hash hsh i) ,path-hsh)))))
    
    ;;All done, so now we check for negative cycles
    ;N-1 cycles are sufficient to enumerate all possible paths
    ;But we run it one additional time, and if something goes down in that last round
    ;then we know we have a negative cycle
    (if (for/or ([node (in-list nodes)])
          ((hsh-get (- MAX-PATH-LENGTH 1) node) . < . (hsh-get (- MAX-PATH-LENGTH 2) node)))
        (begin (when debug (displayln "Negative Cycle found!") (displayln hsh))
               #f)
        `(,(filter-hash hsh (sub1 MAX-PATH-LENGTH)) ,path-hsh))))


(define g (weighted-graph/directed '((5 a c) (10 a b) (20 b c))))
(my-bellman-ford g 'a #t)

(module+ test
  (require rackunit)
  
  (define (hash-equivalent? a b)
    (for/and ([(k v) (in-hash a)])
      (and (hash-has-key? b k)
           (equal? (hash-ref b k) v))))

  (define (check-hash-equivalent? a b)
    (check-true (hash-equivalent? a b)
                (~a "Hashes " a " and " b " are not equivalent!")))

  (define (cleanup hsh)
      (for/hash ([(k v) hsh]
                 #:when (not (= v +inf.0)));exclude no distance
        (values k (inexact->exact v))))
  
  (define (ours-vs-theirs graph start)
    (define-values (their-weights their-path) (bellman-ford graph start))
    (match-define (list our-weights our-path) (my-bellman-ford graph start))
    (check-hash-equivalent? (cleanup our-weights) (cleanup their-weights)))

  (define (my-check nodes start answer)
    (match-let* ([g (weighted-graph/directed nodes)]
                 [(list dist path) (my-bellman-ford g start)])
      (check-hash-equivalent? (cleanup dist) answer)
      (for ([x (in-vertices g)])
        (ours-vs-theirs g x))))
  
  ;simple
  (my-check '((5 a c) (10 a b) (20 b c)) 'a '#hash((b . 10) (c . 5) (a . 0)))
  ;medium
  (my-check '((2 s v) (4 s x) (1 v x) (2 v w) (2 w t) (4 x t)) 's
            '#hash((v . 2)
                   (w . 4)
                   (x . 3)
                   (s . 0)
                   (t . 6)))
  ;stops early
  (my-check '((5 a b) (5 a c) (5 a d) (5 a e)) 'a '#hash((d . 5)
                                                         (a . 0)
                                                         (e . 5)
                                                         (b . 5)
                                                         (c . 5)))
  
  ;negative cycle
  (check-equal? (my-bellman-ford
                 (weighted-graph/directed '((5 a c) (10 a b) (20 b c) (3 c d) (-2 d e) (-2 e c)))
                 'a) #f)

  
  (let ([g (weighted-graph/directed '((1 1 2) (2 1 3) (2 3 4) (3 2 4) (4 2 5) (5 3 6)
                                       (2 6 7) (3 7 8) (8 8 9)))])
    (for ([x (in-vertices g)])
      (ours-vs-theirs g x))))

