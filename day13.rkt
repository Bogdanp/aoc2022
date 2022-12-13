#lang racket/base

(require racket/list
         racket/match
         racket/port
         racket/string)

(define (read-packet in)
  (call-with-input-string (string-replace (read-line in) "," " ") read))

(define pairs
  (call-with-input-file "day13.txt"
    (lambda (in)
      (let loop ([pairs null])
        (cond
          [(eof-object? (peek-byte in))
           (reverse pairs)]
          [else
           (define a (read-packet in))
           (define b (read-packet in))
           (void (read-line in))
           (loop (cons (list a b) pairs))])))))

(define (packet<? a b)
  (match* (a b)
    [((cons (? integer? a-h) a-rest)
      (cons (? integer? b-h) b-rest))
     (if (= a-h b-h)
         (packet<? a-rest b-rest)
         (< a-h b-h))]

    [((cons (? list? a-h) a-rest)
      (cons (? list? b-h) b-rest))
     (define head-res
       (packet<? a-h b-h))
     (if (eq? head-res 'indeterminate)
         (and (= (length a-h)
                 (length b-h))
              (packet<? a-rest b-rest))
         (eq? head-res #t))]

    [((cons (? list?) _)
      (cons (? integer? b-h) b-rest))
     (packet<? a (cons (list b-h) b-rest))]
    [((cons (? integer? a-h) a-rest)
      (cons (? list?) _))
     (packet<? (cons (list a-h) a-rest) b)]

    [('() '()) 'indeterminate]
    [('() _) #t]
    [(_ '()) #f]))

(define part1
  (for/sum ([(p idx) (in-indexed (in-list pairs))] #:when (apply packet<? p))
    (add1 idx)))
(= part1 6086)

(define part2
  (let ([sorted (sort (list* '((2)) '((6)) (apply append pairs)) packet<?)])
    (define d1 (add1 (index-of sorted '((2)))))
    (define d2 (add1 (index-of sorted '((6)))))
    (* d1 d2)))
(= part2 27930)
