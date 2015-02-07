#lang racket
;we provide a contact out as we can use this in deterministic-select
(provide (contract-out
          [quickselect  (->i ([lst (and/c pair? list?)];non-empty list
                              [index exact-positive-integer?])
                             ;next one is optional, so grouped separately
                             ([op (any/c any/c . -> . boolean? )])
                             #:pre/name (lst index)
                             "The specifed index is too large"
                             (not (index . > . (length lst)))
                              [result any/c])]))

;;Determines Nth smallest element (starting at 1) of lst in linear time (average)
;;So this means select's "pos" arg. is 1-index based
;;Also, this is quite short and pretty
(define (quickselect lst pos [op <=])
  (match lst
    [(list a) a]
    [_ (let*-values ([(pivot) (list-ref lst (random (length lst)))]
                     [(left right) (partition (λ (x) (x . op . pivot)) lst)]
                     [(current-statistic) (length left)])
         (cond [(current-statistic . = . pos) pivot]
               [(current-statistic . < . pos) (quickselect right (- pos current-statistic) op)]
               [(current-statistic . > . pos) (quickselect (remove pivot left) pos op)]))]))


(module+ test
  (require rackunit)
  (define LENGTH 50)
  (define MAX-VALUE 200)
  (define NUM-TESTS 50)

  (for ([i NUM-TESTS])
    (let* ([unsorted (build-list LENGTH (λ (_) (random MAX-VALUE)))]
           [statistic (add1 (random (length unsorted)))]
           [selected (quickselect unsorted statistic)])
      (check-equal? selected  ;sub1 b/c statistic uses 1-based indexing, list-ref is 0-based
                    (list-ref (sort unsorted <=) (sub1 statistic))))))
