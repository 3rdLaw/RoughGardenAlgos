#lang racket

#| Input is list of pairs, like this: '((1 3) (4 0) (0 8) (9 9))
   Output is the closest pair: '((8 8) (9 9))

   Runs in N*Log(N) time.
|#
(define (closest-pair lst)
  ;;Looks for closer coord pairs which straddle divide
  (define (closest-split-pair p-x p-y delta)
    (let* ([x-bar (car (list-ref p-x ;get the mid point of the x range
                                 (quotient (length p-x) 2)))]
           ;filter y if its x coordinate is within x-bar +/- delta
           [s-y (filter (λ (z) ((abs ((car z) . - . x-bar)) . <= . delta)) p-y)]
           ;iterate over that range to see if there is a pair of coord closer than delta
           [closer-split-pair (find-closer-split-pair-in-range s-y delta)])

      (if (empty? closer-split-pair)
          `((+inf.0 -inf.0) (-inf.0 +inf.0))
          closer-split-pair)))

  ;;fold over list trying to find a pair that's closer than delta
  (define (find-closer-split-pair-in-range the-range delta)
    (define-values (_ pair)
      (for*/fold ([best delta] [pair '()])
                 ([i (range 0 (length the-range))];linear
                  ;this second loop is run MAX 7 times (why this is O(N) and not O(N^2):
                  [j (range 1 (min 8 (- (length the-range) i)))])
        (let* ([p (list-ref the-range i)]
               [q (list-ref the-range (+ i j))]
               [dist (distance `(,p ,q))])
            (if (dist . < . best)  ;if closer
                (values dist `(,p ,q));set pair to coords
                (values best pair)))));else keep pair empty
    pair)

  #| Main strategy is: 1. recursively divide in half,
                       2. Separately find smallest coord pair on both left and right
                       3. Check if there is a closer pair with one foot in left and right
  |#
  (define (divide p-x p-y)
    (match* (p-x p-y)
      [((list a b) _) p-x];if only 2 points, return them
      [((list a b c) _);brute-force all combinations of 3 points
       (argmin distance `((,a ,b) (,b ,c) (,a ,c)))]
      ;otherwise for more than 3 values, we recurse:
      [ (_ _) (let*-values ([(half-x) (quotient (length p-x) 2)]
                            [(q-x r-x) (split-at p-x half-x)];divide p-x in half
                            [(mid-x) (first (list-ref p-x half-x))];x coord of middle of p-x
                            ;split y based on mid-x
                            [(q-y r-y) (partition (λ (z) ((car z) . <= . mid-x )) p-y)]
                            [(left-pair) (divide q-x q-y)];conqueror left
                            [(right-pair) (divide r-x r-y)];conqueror right
                            [(best-delta-of-non-split-coords) ;get best of left and right
                             (apply min (map distance `(,left-pair ,right-pair)))]
                            ;find closest pair which straddles left and right
                            [(best-split-pair) (closest-split-pair p-x p-y best-delta-of-non-split-coords)])
                ;return the closest across both split & non-split pair sets
                (argmin distance `(,left-pair ,right-pair ,best-split-pair)))]))

  ;function starts executing here!
  ; sort coords by both x & y (n*logn)
  (let ([x-sort (sort lst < #:key car)]
        [y-sort (sort lst < #:key cadr)])
    (divide x-sort y-sort)));start recursive part

;;Euclidean distance between two pairs
;; case-lambda means it accepts the args in a single list, 
;; or as 2 separate params. 
;; E.g.: (distance '((1 2) (3 4))) and (distance '(1 2) '(3 4))
;; both work.
(define distance
  (case-lambda
    [(lst) ;as a single list of two pairs
     (match lst
       [(list (list a-x a-y) (list b-x b-y))
        (sqrt (+ (expt (- a-x b-x) 2)
                 (expt (- a-y b-y) 2)))])]
    [(a b) ;two separate params passed
     (match* (a b)
       [((list a-x a-y) (list b-x b-y))
        (sqrt (+ (expt (- a-x b-x) 2)
                 (expt (- a-y b-y) 2)))])]))

;;N^2
(define (closest-pair-brute-force lst)
  (define-values (pr _);keep pair to return but ignore distance ammount
    (for*/fold ([pr '()] [dist +inf.0])
               ([i (in-list (range 0 (length lst)))]
                [j (in-list (range (add1 i) (length lst)))]
                [i-elem (in-value (list-ref lst i))]
                [j-elem (in-value (list-ref lst j))]
                [cur-dist (in-value (distance i-elem j-elem))]
                #:when  (cur-dist . < . dist))
      (values `(,i-elem ,j-elem) cur-dist)))
  pr)


(define (generate-coord-pairs [x-y-max-value 100] [number-pairs 20])
  (let* ([rand (λ(_) (random x-y-max-value))]
         [x (build-list number-pairs rand)]
         [y (build-list number-pairs rand)]
         [pairs (map list x y)])
    (remove-duplicates pairs)));bound to happen at some point

;; Was just for seeing how 'split-at' works, but kept it 
;;  because it draws a cool-looking pattern
(define (demo-list-splitting)
  (printf "Length:left:right:split   (left-half)|(right-half)\n")
  (for ([x 20])
    (let*-values ([(lst)  (build-list x identity)]
                  [(half) (quotient (length lst) 2)]
                  [(left right) (split-at lst half)])
      (printf "~a:~a:~a:~a:~a   ~a|~a\n" (length lst) (length left) (length right)
              half lst left right))))


(module+ test
  (require rackunit)
  
  (let ([input '((1 2) (2 2) (4 4) (6 6))])
    (check-equal? (sort (closest-pair input) < #:key car)
                  (sort (closest-pair-brute-force input) < #:key car)))
  (let ([input '((1 1) (3 3) (4 5) (6 6) (7 7))])
    (check-equal? (sort (closest-pair input) < #:key car)
                  (sort (closest-pair-brute-force input) < #:key car)))
  (let ([input '((1 1) (3 3) (5 5) (6 6) (8 8) (10 10))])
    (check-equal? (sort (closest-pair input) < #:key car)
                  (sort (closest-pair-brute-force input) < #:key car)))
  (let ([input '((1 1) (3 3) (5 6) (6 7) (8 8) (10 10))])
    (check-equal? (sort (closest-pair input) < #:key car)
                  (sort (closest-pair-brute-force input) < #:key car)))
  (let ([input '((1 1) (3 3) (5 6) (6 7) (8 8) (10 10))])
    (check-equal? (sort (closest-pair input) < #:key car)
                  (sort (closest-pair-brute-force input) < #:key car)))
  
  ;; Try hard to find a test-case that fails
  (define (break-it [num-tries 1000])
    (for* ([x num-tries]
           [lst (in-value (generate-coord-pairs))]
           [bf-answer (in-value (closest-pair-brute-force lst))]
           [fast-answer (in-value (closest-pair lst))]
           ;only collect when a problem 
           #:when (and (not (equal? bf-answer fast-answer))
                       ;and ignore equal paris with swapped-order
                       (not (equal? (reverse bf-answer) fast-answer))
                       ;also exclude different pairs that have equal distance
                       (not (equal? (distance bf-answer) (distance fast-answer))))
           ;quit loop once we have a real problem
           #:final (not (equal? bf-answer fast-answer)))
      (printf "brute-force result, brute-force distance, result, result distance, lst:\n")
      `(,bf-answer ,(distance bf-answer) ,fast-answer ,(distance fast-answer) ,lst)
      (check-equal? bf-answer fast-answer)))
  
  (break-it))
