#lang racket

;; Regular Merge sort n*log(n)
(define (merge-sort lst [op <=])
  (match lst
    [(list a) lst] ;this includes '()
    [_ (let* ([half-of-the-list (quotient (length lst) 2)]
              [left (merge-sort (take lst half-of-the-list))]
              [right (merge-sort (drop lst half-of-the-list))])
         
         (let loop ([lst '()] [left left] [right right])
           (cond [(empty? left)  `(,@lst ,@right)]
                 [(empty? right) `(,@lst ,@left)]
                 [else (match-let ([(list l-first l-rst ...) left]
                                   [(list r-first r-rst ...) right])
                         (if (l-first . op . r-first)
                             (loop `(,@lst ,l-first) l-rst right)
                             (loop `(,@lst ,r-first) left r-rst)))])))]))

;;Still n*log(n)! You can see it's basically merge sort but w/added tallying of inverions.
;; Increasing the constant factor by an additional constant is still linear (so free!)
;;   o(n)+o(n)=o(n).
(define (count-inversions lst)
  (match lst
    [(list a) (values 0 lst)];matches '() too
    [_ (let*-values ([(half-of-the-list) (quotient (length lst) 2)]
                     [(l-count left-half)  (count-inversions (take lst half-of-the-list))]
                     [(r-count right-half) (count-inversions (drop lst half-of-the-list))])

         (let loop ([lst '()] [left left-half] [right right-half] [count (+ l-count r-count)])
           (cond [(empty? left)  (values count `(,@lst ,@right))]
                 [(empty? right) (values count `(,@lst ,@left))]
                 [else (match-let ([(list l-first l-rst ...) left]
                                   [(list r-first r-rst ...) right])
                         (if (l-first . <= . r-first)
                             (loop `(,@lst ,l-first) l-rst right count)
                             (loop `(,@lst ,r-first) left r-rst 
                                   (+ count (length left)))))])))]))

;;n^2
(define (naive-count-inversions lst)
  (for*/sum ([i (length lst)]
             [j (range (add1 i) (length lst))]
             [i-elem (in-value (list-ref lst i))]
             [j-elem (in-value (list-ref lst j))]
             #:when (i-elem . > . j-elem))
    1))

(module+ test
  (require rackunit)
  
  (check-eqv? (naive-count-inversions '(1 2 3 4)) 0)
  (check-eqv? (naive-count-inversions '(1 2 4 3)) 1)
  (check-eqv? (naive-count-inversions '(1 4 2 3)) 2)
  (check-eqv? (naive-count-inversions '(4 1 2 3)) 3)
  (check-eqv? (naive-count-inversions '(1 3 5 2 4 6)) 3)
  (check-eqv? (naive-count-inversions '(1 5 4 8 10 2 6 9 12 11 3 7)) 22)
  
  (let*-values ([(start-lst) '(1 2 3 4)]
                [(correct-count) (naive-count-inversions start-lst)]
                [(cnt end-lst) (count-inversions start-lst )])
    (check-eqv? cnt correct-count)
    (check-equal? end-lst (sort start-lst <)))
  
  (let*-values ([(start-lst) '(1 2 4 3)]
                [(correct-count) (naive-count-inversions start-lst)]
                [(cnt end-lst) (count-inversions start-lst )])
    (check-eqv? cnt correct-count)
    (check-equal? end-lst (sort start-lst <)))
  
  (let*-values ([(start-lst) '(1 4 2 3)]
                [(correct-count) (naive-count-inversions start-lst)]
                [(cnt end-lst) (count-inversions start-lst )])
    (check-eqv? cnt correct-count)
    (check-equal? end-lst (sort start-lst <)))
  
  (let*-values ([(start-lst) '(1 3 5 2 4 6)]
                [(correct-count) (naive-count-inversions start-lst)]
                [(cnt end-lst) (count-inversions start-lst )])
    (check-eqv? cnt correct-count)
    (check-equal? end-lst (sort start-lst <)))
  
  (let*-values ([(start-lst) '(1 5 4 8 10 2 6 9 12 11 3 7)]
                [(correct-count) (naive-count-inversions start-lst)]
                [(cnt end-lst) (count-inversions start-lst )])
    (check-eqv? cnt correct-count)
    (check-equal? end-lst (sort start-lst <)))
  
  ;the confidence-booster
  (let ([the-length 100]
        [num-tries 100])
    (for ([x num-tries])
      (let*-values ([(start-lst) (shuffle (build-list the-length identity))]
                    [(correct-count) (naive-count-inversions start-lst)]
                    [(cnt end-lst) (count-inversions start-lst )])
        (check-eqv? cnt correct-count)
        (check-equal? end-lst (sort start-lst <))))))
