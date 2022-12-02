#lang racket/base

(require racket/match)

(define (char->item c)
  (case c
    [(#\A #\X) 'rock]
    [(#\B #\Y) 'paper]
    [(#\C #\Z) 'scissors]))

(define (item-score it)
  (case it
    [(rock) 1]
    [(paper) 2]
    [(scissors) 3]))

(define (play a b)
  (match* (a b)
    [(it it) 3]
    [('paper 'rock) 6]
    [('scissors 'paper) 6]
    [('rock 'scissors) 6]
    [(_ _) 0]))

(define part1
  (call-with-input-file "day02.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (define p1 (char->item (string-ref line 0)))
        (define p2 (char->item (string-ref line 2)))
        (+ (play p2 p1)
           (item-score p2))))))

(define (item-loser it)
  (case it
    [(rock) 'scissors]
    [(paper) 'rock]
    [(scissors) 'paper]))

(define (item-winner it)
  (case it
    [(scissors) 'rock]
    [(rock) 'paper]
    [(paper) 'scissors]))

(define part2
  (call-with-input-file "day02.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (define p1 (char->item (string-ref line 0)))
        (define p2
          (case (string-ref line 2)
            [(#\X) (item-loser p1)]
            [(#\Y) p1]
            [(#\Z) (item-winner p1)]))
        (+ (play p2 p1)
           (item-score p2))))))
