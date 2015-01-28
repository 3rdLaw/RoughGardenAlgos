#lang racket

;;Not the in-place version, b/c we're functional here in Racket.
;;See Ruby version for imperative version
(define (quick-sort lst [op <=])
  ;picks random element from the list, this keeps it n*log(n) on average
  (define (pivot-random lst)
    (list-ref lst (random (length lst))))
  
  (match lst
    [(list) lst];base cases
    [(list a) lst]
    [_ (let*-values ([(pivot) (pivot-random lst)]
                     [(left right) (partition (λ (x) (x . op . pivot))
                                              #| Note that (remove ...) below might remove an element
                                                  equal to the pivot but isn't actually the pivot.
                                                  But it works out correctly in the end anyways. |#
                                              (remove pivot lst))])
         `(,@(quick-sort left op) ,pivot ,@(quick-sort right op)))]))


(module+ test
  (require rackunit)
  (define LENGTH 50)
  (define MAX-VALUE 200)
  (define NUM-TESTS 500)
  
  (for ([i NUM-TESTS])
    (let* ([unsorted (build-list LENGTH (λ (_) (random MAX-VALUE)))]
           [sorted (quick-sort unsorted)])
      (check-equal? sorted (sort unsorted <=)))))
