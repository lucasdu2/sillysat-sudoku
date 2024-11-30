#lang racket
(provide lit dpll)

;; An extremely bare-bones DPLL-based SAT solver for use in Racket programs.
;; We can define boolean variables in Racket and build up formulas in CNF that
;; we can then pass to the solver logic. We do not handle any sort of parsing
;; or lexing of DIMACS/other input formats in this code. The logic makes no use
;; of advanced optimizations (i.e. watched literals, clause-learning, or
;; activity-based heuristics).

;; Helper structs
;; =============================================================================
;; NOTE: sign must be a boolean (#t or #f) --- #f means negation. var should be
;; represented by an arbitrary positive integer.
;; NOTE: If you don't have #:transparent, we won't be able to easily test
;; equality with (equals?). There are other ways to test struct equality, but
;; this using (equals?) seems to be the most concise.
(struct lit (sign var) #:transparent)

;; Propagation and choice helper functions
;; =============================================================================
;; NOTE: At each stage, we will propagate assignments as follows:
;; - If a literal is false, we will remove all instances of that variable from
;;   each clause of the CNF
;; - If a literal is positive, we will remove all *clauses* containing that
;;   variable from the CNF

;; propagate-assignment takes an assignment for a variable and propagates the
;; effects of that assignment according to the rules outlined above.
(define (propagate-assignment var b-assign cnf)
  (define (propagate-iter in-cnf out-cnf)
    (if (empty? in-cnf)
        out-cnf
        (let ([cl (first in-cnf)]
              [xs (rest in-cnf)])
          (propagate-iter
           xs
           (if (findf (λ (l) (equal? l (lit b-assign var))) cl)
               ;; If the assignment results in a literal that evaluates
               ;; to TRUE, completely remove the clause
               out-cnf
               ;; Remove matching lits from the clause
               (cons
                (filter (λ (l) (not (equal? (lit-var l) var))) cl)
                out-cnf))))))
  (propagate-iter cnf '()))

;; bcp recursively finds a unit clause and then propagates the implication.
(struct bcp-results (cnf assignments))
(define (bcp cnf assignments)
  (define (search-for-unit)
    (findf (λ (clause) (= (length clause) 1)) cnf))
  (let ([found-unit (search-for-unit)])
    (if (not found-unit)
        (bcp-results cnf assignments)
        (let* ([l (first found-unit)]
               [v (lit-var l)])
          ;; NOTE: The implied variable assignment will always be (lit-sign l)
          (bcp
           (propagate-assignment v (lit-sign l) cnf)
           (cons (cons v (lit-sign l)) assignments))))))

;; choose-unassigned-var naively takes the first variable in the CNF.
(define (choose-unassigned cnf)
  (lit-var (first (first cnf))))

;; Functions to determine SAT and UNSAT
;; =============================================================================
;; If all clauses have been removed, we have SAT
(define (sat? f)
  (equal? f '()))
;; If any clause is empty, we have UNSAT
(define (unsat? f)
  (ormap (λ (clause) (equal? clause '())) f))

;; DPLL function
;; =============================================================================
(define (dpll cnf)
  (define (dpll-assignments cnf assignments)
    (let* ([res (bcp cnf '())]
           [res-cnf (bcp-results-cnf res)]
           [res-ass (bcp-results-assignments res)]
           [new-ass (append res-ass assignments)])
      (cond
        [(sat? res-cnf) new-ass]
        [(unsat? res-cnf) #f]
        [else
         (let ([p (choose-unassigned res-cnf)])
           (or
            (dpll-assignments (propagate-assignment p #t res-cnf) (cons (cons p #t) new-ass))
            (dpll-assignments (propagate-assignment p #f res-cnf) (cons (cons p #f) new-ass))))])))
  (dpll-assignments cnf '()))
