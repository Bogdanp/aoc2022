#lang racket/base

(require racket/list
         racket/set)

(define (prio c)
  (cond
    [(and (char>=? c #\a)
          (char<=? c #\z))
     (- (char->integer c) 96)]
    [else
     (+ 26 (- (char->integer c) 64))]))

(define part1
  (call-with-input-file "day03.txt"
    (lambda (in)
      (for/sum ([line (in-lines in)])
        (define items (string->list line))
        (define-values (a b)
          (split-at items (quotient (length items) 2)))
        (define a-set (apply set a))
        (define b-set (apply set b))
        (define dupes
          (set->list (set-intersect a-set b-set)))
        (if (null? dupes) 0 (prio (car dupes)))))))

(define (read-items in)
  (apply set (string->list (read-line in))))

(define part2
  (call-with-input-file "day03.txt"
    (lambda (in)
      (let loop ([total 0])
        (cond
          [(eof-object? (peek-char in))
           total]
          [else
           (define e0 (read-items in))
           (define e1 (read-items in))
           (define e2 (read-items in))
           (define common
             (set->list (set-intersect e0 e1 e2)))
           (loop (+ total (prio (car common))))])))))
