#lang racket/base

(require racket/list)

(define elves
  (call-with-input-file "day01.txt"
    (lambda (in)
      (for/fold ([elves (hasheq)] [seq 1] #:result (sort (hash->list elves) #:key cdr >))
                ([line (in-lines in)])
        (cond
          [(string=? "" line)
           (values elves (add1 seq))]
          [else
           (define num (string->number line))
           (values (hash-update elves seq (λ (n) (+ n num)) 0) seq)])))))

(define part1
  (cdar elves))

(define part2
  (apply + (map cdr (take elves 3))))
