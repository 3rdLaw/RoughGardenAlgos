#lang racket
(require library/library)

;;Determines Nth smallest element (starting at 1) of lst in linear time deterministically
;;So this means select's "pos" arg. is 1-index based
(define (select-non-random lst index [op <=])
  #|The best pivot would be the median- i.e., an application of select-non-random: circular!
  Since we can't call this algorithm as a subroutine (without blowing up our run-time),
  we approximate through the "median of medians".|#

  ;;This is the Median of means sub-routine:
  (define (choose-pivot lst)
    (let* ([sub-lsts (chunk-uneven lst 5)];split into many groups of length=5
           #|Sorting elements of <=5 takes constant time! |#
           [sorted-sub-lsts (map (curryr sort op) sub-lsts)];sort each group of 5
           [medians (map get-middle sorted-sub-lsts)];get the medians from sublists
           ;recursively call our outer function with the medians
           [pivot (select-non-random medians (quotient (length medians) 2))])
      pivot))

  (match lst
    [(list) (error "Empty list passed to select-non-random")]
    ;base case: we sort to solve lists w/length <= 10
    [(? list? (app (λ (x) ((length x) . <= . 10))
                   #t))
     (list-ref (sort lst op) (sub1 index))]

    [_  (let*-values ([(pivot) (choose-pivot lst)]
                      [(left right) (partition (λ (x) (x . op . pivot)) lst)]
                      [(current-statistic) (length left)])
          (cond [(current-statistic . = . index) pivot]
                [(current-statistic . < . index) (select-non-random right (- index current-statistic) op)]
                [(current-statistic . > . index) (select-non-random (remove pivot left) index op)]))]))


;;returns middle element of list
(define (get-middle lst)
  (cond [(empty? lst)        lst]
        [(odd? (length lst)) (list-ref lst (ceiling (quotient (length lst) 2)))]
        [else                (list-ref lst (sub1    (quotient (length lst) 2)))]))

;;Compare speed between random and non-random versions.
;;Randomized is generally at least 3x faster
(require "quickselect.rkt")
(let* ([ LENGTH 150] [MAX-VALUE 180] [NUM-TESTS 10000])
  (for/fold ([rand 0] [determ 0])
            ([x (in-list (build-list NUM-TESTS ;each list is a list of numbers and an index
                                     (λ (_) `(,(build-list LENGTH (λ (_) (random MAX-VALUE)))
                                              ,(add1 (random LENGTH))))))])
    (match-define (list (list lst ...) index) x)

    (match-define-values (r-res r-cpu-time r-real r-gc)
                         (time-apply quickselect  `(,lst ,index)))
    (match-define-values (d-res d-cpu-time d-real d-gc)
                         (time-apply select-non-random  `(,lst ,index)))
    ;;for sanity
    (when (not (equal? r-res d-res)) (error "Unequal results!"))

    (values (+ rand   r-real)
            (+ determ d-real))))


(module+ test
  (require rackunit)

  (check-equal? (get-middle '(1 2 3 4 5)) 3)
  (check-equal? (get-middle '(1 2 3 4 5 6)) 3)

  (define LENGTH 50)
  (define MAX-VALUE 200)
  (define NUM-TESTS 50)

  (for ([i NUM-TESTS])
    (let* ([unsorted (build-list LENGTH (λ (_) (random MAX-VALUE)))]
           [statistic (add1 (random (length unsorted)))]
           [selected (select-non-random unsorted statistic)])
      (check-eq? selected  ;sub1 b/c statistic uses 1-based indexing, but list-ref is 0-based
                 (list-ref (sort unsorted <=) (sub1 statistic))))))
