#lang racket
;;assumes vertices are numbers!
(require graph "bellman_ford.rkt" "dijkstras.rkt")

;;O(mn*log(n))
(define (johnsons graph)
  (let/cc bail
    (define new-graph (graph-copy graph))
    (define new-node (add1 (length (get-vertices graph))))

    ;; Add paths to every node from new-node
    ;O(n)
    (for ([node (in-list (get-vertices graph))])
      (add-directed-edge! new-graph new-node node 0))

    ;; Run BF
    ;O(mn)
    (define results (my-bellman-ford new-graph new-node))
    ;return if Bellman-Ford detects negative cycle
    (when (not results) (bail #f))
    
    (match-define (list dist path) results)
    (define (lookup v)
      (hash-ref dist v (λ () (error (format "unexpected lookup for ~a" v)))))

    ;;Corrects weights back to original graph
    ;O(n), called n times, so O(n^2) total for this function's applications
    (define (update-weights head hsh)
      (for/hash ([(tail old-weight) (in-hash hsh)])
        (values tail (+ old-weight (* -1 (lookup head)) (lookup tail)))))

    ;Remove new-node
    (remove-vertex! new-graph new-node)

    ;Update edges to new weights
    ;O(m)
    (for ([edge (in-list (get-edges graph))])
      (match-define (list head tail) edge)
      (define old-weight (apply edge-weight graph edge))
      (apply remove-directed-edge! new-graph edge)
      (add-directed-edge! new-graph head tail (+ old-weight (lookup head) (* -1 (lookup tail)))))

    ;Run dijkstra's repeatedly
    ;O(n*mlog(n))
    (for/list ([node (get-vertices graph)])
      (define-values (dist path) (my-dijkstra new-graph node))
      `(,node ,(update-weights node dist) ,path))))


(johnsons (weighted-graph/directed '((5 1 3) (10 1 2) (20 2 3) (8 3 4))))

