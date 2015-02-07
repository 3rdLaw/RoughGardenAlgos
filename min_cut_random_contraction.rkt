#lang racket
;used to mark the end of a node's name
(define SUFFIX "$")

;; Random Contraction Min Cut algorithgm
;; It group vertices into two groups, hopefully w/min. # of edge crossings.
(define (random-contraction adj-lst)

  ;We call remove-dups to ensure we have no dups in our edge listing
  ;We also add a suffix in case node's are numbers (without the suffix,
  ; it can be unclear which nodes were concatentated to form the given node):
  ; for example, if you have nodes 1-25, is 21 a single node or 2 and 1 merged?
  (let loop ([edges (add-suffix-to-each-vertex (remove-dups adj-lst))])
    (cond [(num-vertices<=2? edges) (show-cut edges adj-lst)]
          ;select and edge at random
          [else (let* ([selected-edge (list-ref edges (random (length edges)))]
                       ;name the new edge
                       [new-edge (make-new-edge selected-edge)]
                       ;merge the list of edges
                       [merged-lst (merge-edges selected-edge edges new-edge)]
                       ;remove any self loops
                       [merged-no-loops (remove* `((,new-edge ,new-edge)) merged-lst)])
                  (loop merged-no-loops))])));repeat

;; '(a b) -> 'ab and '(12$ 2$) -> '12$2$
(define (make-new-edge lst)
  (string->symbol (string-join (map symbol->string lst) "")))

(define (num-vertices lst)
  (length (remove-duplicates (flatten lst))))

(define (num-vertices<=2? lst)
  ((num-vertices lst) . <= . 2))

;;replace a squashed vertex inside a pair in place, if needed
(define (replace-inside-pair strt end replacement pair)
  (match pair ;it's not every day you get to do list-no-order matching!
    [(list-no-order (== strt) (== end)) `(,replacement ,replacement)]
    [(list a (== strt))                 `(,a ,replacement)]
    [(list (== strt) a)                 `(,replacement ,a)]
    [(list a (== end))                  `(,a ,replacement)]
    [(list (== end) a)                  `(,replacement ,a)]
    [_                                  pair]));else return as is

;;updates edges list to reflect merging
(define (merge-edges selected-edge edges new-edge)
  (match-define (list strt end) selected-edge);name old edge's parts
  ;call replace-inside-pair for each edge
  (map (λ (x) (replace-inside-pair strt end new-edge x))
       edges))

;;'abc -> '(a b c)
(define (symbol->list-of-symbols sym)
  (map string->symbol
       (string-split (symbol->string sym) SUFFIX)))

;;Removes both vanilla duplicates and reversed-pair duplicates
;;little schemer style
(define (remove-dups lst)
  (letrec ([recur
            (lambda (cur store)
              (cond [(empty? cur) store]
                    [else (match-define (list fir rst ...) cur)
                          (if (or (member fir store)
                                  (member (reverse fir) store))
                              (recur rst store)
                              (recur rst (cons fir store)))]))])
    (reverse
     (recur lst '()))))

;;Removes suffixes
(define (strip-out-suffixes str)
  (string->symbol (regexp-replace* (regexp-quote SUFFIX)
                                   (symbol->string str)
                                   "")))

;returns number, cut, and  crossing edges in a list
(define (show-cut lst adj-lst [remove-suffixes? #f])
  (let* (;sort/removeDups to ensure we turn '((a b) (b a)) into just '((a b))
         [soln-list (remove-duplicates
                     (map (curryr sort symbol<?)
                          lst))]
         [soln (first soln-list)];grab first, there should only be one
         [lst-num-edges (num-crossing-edges soln adj-lst)]
         [cut (if remove-suffixes? ;strip suffixes to make it pretty?
                  (map strip-out-suffixes soln)
                  soln)])
    ;match on num-crossing-edges' two results in the list
    (match-define (list num-edges edges) lst-num-edges)
    `(,num-edges ,edges ,cut)))


;;returns # of edge crossings & crossing-edges between given cut
(define (num-crossing-edges soln adj-lst)
  (match-define (list left right) soln)
  (let* ([no-dups-adj-lst (remove-dups adj-lst)]
         [left-lst  (symbol->list-of-symbols left)]
         [right-lst (symbol->list-of-symbols right)]
         ;we check for both '(a b) and '(b a), thus the append below
         [possibs (append (cartesian-product `(,left-lst ,right-lst))
                          (cartesian-product `(,right-lst ,left-lst)))]
         #|Given our cut, we use cartesian products to build up a list
         of every possible edge, and check if it exists in our input|#
         [edges-present (remove-duplicates
                         (filter (λ (x) (member x no-dups-adj-lst))
                                 possibs))])
    `(,(length edges-present) ,edges-present)))

;; Takes as input an undirected graph in adjacency list form.
;; It repeatedly calls (random-contraction) to find a min
(define (min-cut graph)
  (define num-of-vertices (num-vertices graph))
  ;This # of iterations sets our chance to not find best result to (1/n)
  ;where n is # vertices
  (define iterations
    (inexact->exact (round (* (sqr num-of-vertices) (log num-of-vertices)))))

  (for/fold ([cut-length +inf.0]
             [cut '()]
             [edges '()])
            ([_ iterations])
    (match-define (list cur-cut-length cur-edges cur-cut) (random-contraction graph))
    (if (cur-cut-length . < . cut-length)
        (values cur-cut-length cur-edges cur-cut)
        (values cut-length edges cut))))

;;Adds the suffix to each edge component in the lst
(define (add-suffix-to-each-vertex lst)
  (map (λ (x) ;each edge is a pair so we map inside x
         (map (λ (y)  (string->symbol (string-append (symbol->string y)
                                                     SUFFIX)))
              x))
       lst))

;;'(a b c d) -> '((a b) (a c) (a d))
(define (pairify-lst x)
  (match-define (list hd tl ...) x)
  (map (λ (y) `(,hd ,y))
       tl))

;cartesian product of a list
(define (cartesian-product lst)

  ;adds x to the beginning of each list in lst
  (define (stitch lst x)
    (map (lambda (each) (cons x each))
         lst))

  (foldr (λ (sublst rst)
           (append-map (curry stitch rst)
                       sublst))
         '(())
         lst))

;;Loads & parses string test examples
(define (parse-str-to-adj-list str)
  (let* (;split on newline
         [lines (string-split str "\n")]
         ;space delimited
         [lsts (map (curryr string-split " ") lines)]
         ;turn into symbols
         [num-lsts (map (λ (x) (map string->symbol x)) lsts)])
    (append* (map pairify-lst num-lsts))))

(define g1 "1 19 15 36 23 18 39
2 36 23 4 18 26 9
3 35 6 16 11
4 23 2 18 24
5 14 8 29 21
6 34 35 3 16
7 30 33 38 28
8 12 14 5 29 31
9 39 13 20 10 17 2
10 9 20 12 14 29
11 3 16 30 33 26
12 20 10 14 8
13 24 39 9 20
14 10 12 8 5
15 26 19 1 36
16 6 3 11 30 17 35 32
17 38 28 32 40 9 16
18 2 4 24 39 1
19 27 26 15 1
20 13 9 10 12
21 5 29 25 37
22 32 40 34 35
23 1 36 2 4
24 4 18 39 13
25 29 21 37 31
26 31 27 19 15 11 2
27 37 31 26 19 29
28 7 38 17 32
29 8 5 21 25 10 27
30 16 11 33 7 37
31 25 37 27 26 8
32 28 17 40 22 16
33 11 30 7 38
34 40 22 35 6
35 22 34 6 3 16
36 15 1 23 2
37 21 25 31 27 30
38 33 7 28 17 40
39 18 24 13 9 1
40 17 32 22 34 38" )

(min-cut (parse-str-to-adj-list g1))


;;Many tests for this one
(module+ test
  (require rackunit)

  (check-eqv? (num-vertices '((a b) (b c) (d c) (a c) (c e))) 5)

  (check-eqv? (num-vertices<=2? '((a b) (b c) (d c) (a c))) #f)
  (check-eqv? (num-vertices<=2? '((a b) (b a))) #t)

  (check-equal? (make-new-edge '(b c)) 'bc)
  (check-equal? (make-new-edge '(bdf cd)) 'bdfcd)

  (check-equal? (replace-inside-pair 'a 'b 'ab '(a b)) '(ab ab))
  (check-equal? (replace-inside-pair 'a 'b 'ab '(b a)) '(ab ab))
  (check-equal? (replace-inside-pair 'a 'b 'ab '(b g)) '(ab g))
  (check-equal? (replace-inside-pair 'a 'b 'ab '(g b)) '(g ab))
  (check-equal? (replace-inside-pair 'a 'b 'ab '(a g)) '(ab g))
  (check-equal? (replace-inside-pair 'a 'b 'ab '(g a)) '(g ab))

  (check-equal? (merge-edges '(a b) '((a b) (b c) (c d) (a c) (d e)) 'ab)
                '((ab ab) (ab c) (c d) (ab c) (d e)))

  (check-equal? (symbol->list-of-symbols 'a$b$c$) '(a b c))
  (check-equal? (symbol->list-of-symbols 'a$) '(a))

  (check-equal? (remove-dups '((a c) (c a) (a c) (c d) (b d)))
                '((a c) (c d) (b d)))

  (check-equal? (add-suffix-to-each-vertex '((a b) (a c) (b c) (b d) (c d)))
                '((a$ b$) (a$ c$) (b$ c$) (b$ d$) (c$ d$)))

  (match-let ([(list num edges) (num-crossing-edges '(a$b$c$ d$)
                                                    '((a b) (a c) (b c) (b d) (c d)))])
    (check-eq? num 2))

  (match-let ([(list num edges) (num-crossing-edges '(a$b$ c$d$)
                                                    '((a b) (a c) (b c) (b d) (c d)))])
    (check-eq? num 3))


  (match-let ([(list num edgs) (num-crossing-edges '(a$b$c$d$ w$x$y$z$)
                                                   '((a b) (a d) (a c) (b c) (b d) (c d)
                                                     (b w) (d y)
                                                     (w x) (w z) (w y) (x y) (x z) (y z)))])
    (check-eq? num 2))

  (match-let ([(list num edges) (num-crossing-edges '(a$ b$c$d$w$x$y$z$)
                                                    '((a b) (a d) (a c) (b c) (b d) (c d)
                                                      (b w) (d y)
                                                      (w x) (w z) (w y) (x y) (x z) (y z)))])
    (check-eq? num 3))

  (match-let ([(list num edges) (num-crossing-edges '(d$ a$b$c$w$x$y$z$)
                                                    '((a b) (a d) (a c) (b c) (b d) (c d)
                                                      (b w) (d y)
                                                      (w x) (w z) (w y) (x y) (x z) (y z)))])
    (check-eq? num 4))

  (match-let ([(list num crossings cut) (show-cut '((a$b$ c$d$) (a$b$ c$d$))
                                                  '((a b) (a c) (b c) (b d) (c d))
                                                  #t)])
    (check-equal? (list num cut)
                  '(3 (ab cd))))

  (match-let ([(list num crossings cut) (show-cut '((a$b$c$ d$) (d$ a$b$c$))
                                                  '((a b) (a c) (b c) (b d) (c d))
                                                  #t)])
    (check-equal? (list num cut)
                  '(2 (abc d))))

  (check-equal? (pairify-lst '(a b c d))
                '((a b) (a c) (a d)))

  (define g2 "1 2 3 4 5
2 3 4 1
3 4 1 2
4 1 2 3 8
5 1 6 7 8
6 7 8 5
7 8 5 6
8 4 6 5 7")

  (match-let-values ([(num cut crossings)  (min-cut (parse-str-to-adj-list g2))])
                    (check-eqv? 2 num)))
