#lang racket
(require "sillysat.rkt")
(provide board solve)

;; Helper structs and related functions
;; =============================================================================
(struct coord (row col) #:transparent)
;; NOTE: To make things a bit easier implementation-wise, we will encode a
;; "real" guess number g as (g-1) in our variable calculation. This allows us
;; to use the arithmetic-based encoding specified below. We just need to be
;; careful to encode the board givens and decode the solver result properly.
;; More specifically: if we have a board of size=3, n=9, the real guess 1 will
;; be encoded as num=0 (and so on). An empty cell, represented by the guess
;; number 0, can be encoded as n=9. This should not be relevant though, given
;; our constraint encoding strategy.
(struct guess (row col num) #:transparent)

;; setup is the initial state of the board and should be a list of lists. We
;; choose a list over a vector because it's just the more natural functional
;; data structure in Racket.
;; size is the size of the board in terms of boxes, i.e. a 9x9 board has size 3.
(struct board (setup size))

(define (valid-board? b)
  (let* ([size (board-size b)]
         [setup (board-setup b)]
         [n (* size size)])
    (and (equal? (length setup) n)
         (andmap (λ (r) (equal? (length r) n)) setup))))

;; Logic to encode guesses as integer variables for our solver
;; =============================================================================
;; NOTE: Another solution to this would be to have a map of variables to guesses
;; that gets populated when we encode everything and that we check against when
;; we get a solution from our solver. The approach below --- using arithmetic to
;; deterministically represent guesses as variables (and vice versa) --- seems
;; a bit more efficient though (at least space-wise, probably time-wise too).
(define (encode-guess-as-var guess n)
  (let ([row (guess-row guess)]
        [col (guess-col guess)]
        [num (guess-num guess)])
    (+ num (* n (+ (* n row) col)))))

(define (decode-guess-as-var var n)
  (let* ([num (modulo var n)]
         [col (modulo (quotient var n) n)]
         [row (modulo (quotient (quotient var n) n) n)])
    (guess row col num)))

(define (get-var-from-coord-num c num n)
  (encode-guess-as-var (guess (coord-row c) (coord-col c) num) n))

;; Coordinate generation functions
;; =============================================================================
(define (generate-row-coords n)
  (map (lambda (r) (map (λ (c) (coord r c)) (range n))) (range n)))

(define (generate-col-coords n)
  (map (lambda (c) (map (λ (r) (coord r c)) (range n))) (range n)))

;; (0 0) (0 1) (0 2)
;; (1 0) (1 1) (1 2)
;; (2 0) (2 1) (2 2)
(define (generate-one-box rows-columns-pair)
  (let ([rows (first rows-columns-pair)]
        [cols (second rows-columns-pair)])
    (append* (map (λ (r) (map (λ (c) (coord r c)) rows)) cols))))

(define (generate-rows-columns-pairs size)
  ;; A crude, but functional, way to get some sublists
  (let* ([full-range (range (* size size))]
         [split-range (map
                      (λ (i) (take (drop full-range (* i size)) size))
                      (range size))])
    ;; NOTE: append* is a nice convenience.
    (append* (map (λ (rs) (map (λ (cs) (list rs cs)) split-range)) split-range))))

(define (generate-box-coords size)
  (map generate-one-box (generate-rows-columns-pairs size)))

;; Encoding functions for constraints, parameterized on lists of coordinates
;; =============================================================================
;; To encode the constraint that there should be at least 1 of each number in
;; a set of coordinates, we can use a conjunction of disjunctions, where each
;; disjunctive clause consists of guesses for a certain number at each
;; coordinate in the set.
(define (encode-atleast-one n coords)
  (map
   (λ (g-num)
     (map
      (λ (c) (lit #t (get-var-from-coord-num c g-num n)))
      coords))
   (range n)))
;; To encode the constraint that there should be *at most* one of each number in
;; a set of coordinates, we can construct a conjunction of pairwise disjunctions
;; for each possible number in the set of coordinates. For example, to encode
;; the fact that there can be at most one of the number 9 in row 0:
;; (
;;   ((lit #t (guess 0 0 9)) ∨ (lit #t (guess 0 1 9))) ∧
;;   ((lit #t (guess 0 0 9)) ∨ (lit #t (guess 0 2 9))) ∧
;;   ...and so on
;; )
(define (get-all-pairs l)
  (define (pairs-iter rem-l pairs)
    (if (empty? rem-l)
        pairs
        (pairs-iter (rest rem-l)
                    (append
                     (map (λ (c) (list (first rem-l) c)) (rest rem-l))
                     pairs))))
  (pairs-iter l '()))

;; TODO: Write up the thinking behind the encoding of this condition.
(define (encode-maximum-one n coords)
  (append*
   (map
    (λ (pair)
      (let ([fst (first pair)]
            [snd (second pair)])
        (map (λ (g-num) (list
                         (lit #f (get-var-from-coord-num fst g-num n))
                         (lit #f (get-var-from-coord-num snd g-num n))))
             (range n))))
    (get-all-pairs coords))))

;; This encodes the constraint that each cell/coordinate can only have a single,
;; unique assignment.
(define (unique-assignment n c)
  (map (λ (pair) (list
                  (lit #f (get-var-from-coord-num c (first pair) n))
                  (lit #f (get-var-from-coord-num c (second pair) n))))
       (get-all-pairs (range n))))
(define (encode-unique-assignment n)
  (append*
   (map
    (λ (row-idx)
      (append*
       (map
        (λ (col-idx) (unique-assignment n (coord row-idx col-idx)))
        (range n))))
    (range n))))

;; Any non-zero cell in the initial setup should be encoded as a unit clause.
;; NOTE: Remember that we encode a "real" guess g as (g-1) in our variable
;; calculation.
(define (encode-givens n setup)
  (append*
   (map
    (λ (row row-idx)
      (foldl
       (λ (given col-idx row-nonzeros)
         (if (equal? given 0)
             row-nonzeros
             (cons
              ;; Encode as a unit clause, decrementing givens appropriately
              (list (lit #t (encode-guess-as-var
                             (guess row-idx col-idx (- given 1))
                             n)))
              row-nonzeros)))
       '()
       row
       (range n)))
    setup
    (range n))))

;; Solve the puzzle
;; =============================================================================
;; Some helper functions to generate the actual solution board from the solver's
;; response.
(define (board-from-solver-model solver-model n)
  (map
   ;; Sort each unordered row by column, from least to greatest. Then we take
   ;; the actual number solution for each cell and join them all into a list.
   ;; This should fully construct the solution board.
   (λ (solution-row)
     ;; Make sure that we increment the returned number to get the real guess
     ;; (see note on guess encoding at top of file).
     (map (λ (g) (+ (guess-num g) 1))
          (sort solution-row (λ (g1 g2) (< (guess-col g1) (guess-col g2))))))
   ;; Construct board so that it's ordered by row. Within each row, the columns
   ;; are still unordered (which we address with the map above).
   (map (λ (row-idx)
         (filter (λ (g) (equal? (guess-row g) row-idx)) solver-model))
       (range n))))

(define (construct-solution-from-sat n assignments)
  ;; The second field of the assignment pair is the #t or #f assignment.
  (define (filter-for-true assignment) (cdr assignment))
  (if (not assignments)
      #f
      (board-from-solver-model
       (map (λ (a) (decode-guess-as-var (car a) n)) (filter filter-for-true assignments))
       n)))

;; Call the solver on our encodings
(define (solve board)
  (let* ([size (board-size board)]
         [n (* size size)]
         [setup (board-setup board)]
         [row-coords (generate-row-coords n)]
         [col-coords (generate-col-coords n)]
         [box-coords (generate-box-coords size)]
         [all-coords (append row-coords col-coords box-coords)])
    (define (apply-encoding-fun encoding-fun)
      (append* (map (curry encoding-fun n) all-coords)))
    (construct-solution-from-sat
     n
     (dpll
      (append
       (encode-givens n setup)
       (encode-unique-assignment n)
       (apply-encoding-fun encode-atleast-one)
       (apply-encoding-fun encode-maximum-one))))))
