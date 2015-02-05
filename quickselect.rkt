#lang racket
;;Determines Nth smallest element (starting at 1) of lst in linear time (average)
;;So this means select's "pos" arg. is 1-index based
;;Also, this is quite short and pretty
(define (select lst pos [op <=])
  (match lst
    [(list) (error "Empty array passed to select")]
    [(list a) a]
    [_ (let*-values ([(pivot) (list-ref lst (random (length lst)))]
                     [(left right) (partition (λ (x) (x . op . pivot)) lst)]
                     [(current-statistic) (length left)])
         (cond [(current-statistic . = . pos) pivot]
               [(current-statistic . < . pos) (select right (- pos current-statistic) op)]
               [(current-statistic . > . pos) (select (remove pivot left) pos op)]))]))


(module+ test
  (require rackunit)
  (define LENGTH 50)
  (define MAX-VALUE 200)
  (define NUM-TESTS 50)

  (for ([i NUM-TESTS])
    (let* ([unsorted (build-list LENGTH (λ (_) (random MAX-VALUE)))]
           [statistic (add1 (random (length unsorted)))]
           [selected (select unsorted statistic)])
      (check-equal? selected  ;sub1 b/c statistic uses 1-based indexing, list-ref is 0-based
                    (list-ref (sort unsorted <=) (sub1 statistic))))))
