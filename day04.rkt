#lang racket/base

(require racket/match)

(define (range-contains? a b)
  (and (>= (car b) (car a))
       (<= (cdr b) (cdr a))))

(define (range-overlaps? a b)
  (or (and (>= (car b) (car a))
           (<= (car b) (cdr a)))
      (and (>= (cdr b) (car a))
           (<= (cdr b) (cdr a)))))

(define (solve p)
  (call-with-input-file "day04.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (match line
          [(regexp #rx"([0-9]+)-([0-9]+),([0-9]+)-([0-9]+)"
                   (list _
                         (app string->number a-lo)
                         (app string->number a-hi)
                         (app string->number b-lo)
                         (app string->number b-hi)))
           (define a (cons a-lo a-hi))
           (define b (cons b-lo b-hi))
           (if (p a b)
               1
               0)])))))

(define part1
  (solve (λ (a b)
           (or (range-contains? a b)
               (range-contains? b a)))))

(define part2
  (solve (λ (a b)
           (or (range-overlaps? a b)
               (range-overlaps? b a)))))
