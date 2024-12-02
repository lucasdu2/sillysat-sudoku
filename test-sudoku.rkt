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

(define simple-sudoku
  (board
   (list
    (list 0 0 8 0 9 3 5 0 0)
    (list 6 5 0 4 0 2 0 0 0)
    (list 0 2 1 0 8 0 3 0 0)
    (list 3 8 0 0 6 0 2 0 9)
    (list 0 7 0 0 0 0 0 1 0)
    (list 1 0 9 0 4 0 0 7 3)
    (list 0 0 5 0 1 0 7 3 0)
    (list 0 0 0 3 0 9 0 2 6)
    (list 0 0 6 8 2 0 1 0 0))
   3))

(define medium-sudoku
  (board
   (list
    (list 0 2 0 8 5 4 0 0 0)
    (list 0 0 0 6 0 0 0 0 8)
    (list 0 1 0 0 0 0 0 0 9)
    (list 2 0 0 0 0 0 0 9 3)
    (list 0 7 5 3 0 8 6 2 0)
    (list 8 9 0 0 0 0 0 0 7)
    (list 4 0 0 0 0 0 0 6 0)
    (list 3 0 0 0 0 2 0 0 0)
    (list 0 0 0 7 6 1 0 4 0))
   3))

(define hard-sudoku
  (board
   (list
    (list 0 0 0 0 5 0 9 0 0)
    (list 9 8 0 0 0 0 0 5 0)
    (list 0 4 5 7 0 0 3 0 0)
    (list 0 0 4 2 0 0 8 7 0)
    (list 0 0 0 1 0 4 0 0 0)
    (list 0 7 6 0 0 9 1 0 0)
    (list 0 0 8 0 0 1 7 3 0)
    (list 0 2 0 0 0 0 0 8 1)
    (list 0 0 7 0 3 0 0 0 0))
   3))

(define unsolvable-sudoku
  (board
   (list
    (list 2 0 0 9 0 0 0 0 0)
    (list 0 0 0 0 0 0 0 6 0)
    (list 0 0 0 0 0 1 0 0 0)
    (list 5 0 2 6 0 0 4 0 7)
    (list 0 0 0 0 0 4 1 0 0)
    (list 0 0 0 0 9 8 0 2 3)
    (list 0 0 0 0 0 3 0 8 0)
    (list 0 0 5 0 1 0 0 0 0)
    (list 0 0 7 0 0 0 0 0 0))
  3))
