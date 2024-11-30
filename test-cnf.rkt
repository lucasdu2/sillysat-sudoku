#lang racket
(require "sillysat.rkt")

;; SAT
(define test1 (list (list (lit #t 1)) (list (lit #f 1) (lit #t 2))))
(define test2 (list (list (lit #t 1) (lit #t 2)) (list (lit #f 3) (lit #f 2) (lit #t 4))))

;; UNSAT
(define test3 (list (list (lit #t 1)) (list (lit #f 1))))
