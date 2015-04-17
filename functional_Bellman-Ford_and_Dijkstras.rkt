#lang racket

(require graph)

; O(V*E)
(define (my-bellman-ford g v) 
  ;|V|, but this gets dropped with the V*E multiplication below
  (define distance-hash (foldl (λ (x res) (if (equal? x v)
                                              (hash-set res x 0)
                                              (hash-set res x +inf.0)))
                               (hash)
                               (get-vertices g)))
  ;|V|
  (for/fold ([dist distance-hash] [path (hash)])
            ([_ (sub1 (length (get-vertices g)))]) ;we don't use this counter
    
    (for/fold ([dist dist] [path path])
              ;|E|
              ([x (get-edges g)])
      
      (match-define (list from to) x)
      (if (< (+ (edge-weight g from to)
                (hash-ref dist from))
             (hash-ref dist to))
          (values (hash-set dist to (+ (edge-weight g from to) 
                                       (hash-ref dist from)))
                  (hash-set path to from))
          (values dist path)))))

;O(V*V)
(define (my-dijkstra g v)
  ;-> two hashes: first returns distance, second is fastest path lookup hash
  (define relax 
    (match-lambda** 
     [((list from to) (list dist path))
      (if ((+ (edge-weight g from to) (hash-ref dist from)) . < . (hash-ref dist to))
          `(,(hash-set dist to ((edge-weight g from to) . + .(hash-ref dist from))) 
            ,(hash-set path to from))
          (list dist path))]))
  
  ;|E|, but this gets dropped with the V multiplication
  (define distance-hash (foldl (λ (x res) (if (equal? x v)
                                              (hash-set res x 0)
                                              (hash-set res x +inf.0)))
                               (hash)
                               (get-vertices g)))
  ;|V|
  (let loop ([vertices (get-vertices g)] [dist distance-hash] [path (hash)])
    (cond [(empty? vertices) (values dist path)]
          [else ;|V|
           (let ([cur-node (argmin (λ (v) (hash-ref dist v)) vertices)])
             (match-define (list d p) (foldl relax
                                             (list dist path)
                                             (map (λ (nd) (list cur-node nd))
                                                  ;constant time (adjacency-list is a hash)
                                                  (get-neighbors g cur-node))))
             (loop (remove cur-node vertices) d p))])))

(module+ test
  (require rackunit)
  (define g (weighted-graph/directed '((1 a b) (3 b c) (2 a c) (3 c e) (2 d e) (4 b d))))
  (check-equal? (call-with-values (λ () (my-bellman-ford g 'a)) list)
                (call-with-values (λ ()     (my-dijkstra g 'a)) list)))
