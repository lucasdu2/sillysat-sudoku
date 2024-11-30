#lang racket
(require "sudoku-solver.rkt")

(define test-board1
  (board
   (list
    (list 2 5 0 0 3 0 9 0 1)
    (list 0 1 0 0 0 4 0 0 0)
    (list 4 0 7 0 0 0 2 0 8)
    (list 0 0 5 2 0 0 0 0 0)
    (list 0 0 0 0 9 8 1 0 0)
    (list 0 4 0 0 0 3 0 0 0)
    (list 0 0 0 3 6 0 0 7 2)
    (list 0 7 0 0 0 0 0 0 3)
    (list 9 0 3 0 0 0 6 0 4))
   3))
