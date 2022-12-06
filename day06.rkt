#lang racket/base

(define (no-duplicates? vec start stop)
  (define entries
    (for/hasheqv ([e (in-vector vec start stop)])
      (values e #t)))
  (= (hash-count entries)
     (- stop start)))

(define (find-marker* in len)
  (define chars (list->vector (string->list (read-line in))))
  (for/first ([i (in-range (sub1 len) (vector-length chars))]
              #:when (no-duplicates? chars (- i (sub1 len)) (add1 i)))
    (add1 i)))

(define (find-sop-marker in)
  (find-marker* in 4))

(define (find-som-marker in)
  (find-marker* in 14))

(module+ test
  (require rackunit)

  (define-check (check-marker proc s expected)
    (define m (proc (open-input-string s)))
    (unless (= m expected)
      (with-check-info
        (['value m]
         ['expected expected])
        (fail-check))))

  (check-marker find-sop-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 7)
  (check-marker find-sop-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 5)
  (check-marker find-sop-marker "nppdvjthqldpwncqszvftbrmjlhg" 6)
  (check-marker find-sop-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 10)
  (check-marker find-sop-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 11)
  (check-marker find-som-marker "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 19)
  (check-marker find-som-marker "bvwbjplbgvbhsrlpgdmjqwftvncz" 23)
  (check-marker find-som-marker "nppdvjthqldpwncqszvftbrmjlhg" 23)
  (check-marker find-som-marker "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 29)
  (check-marker find-som-marker "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 26))

(define part1 (call-with-input-file "day06.txt" find-sop-marker))
(define part2 (call-with-input-file "day06.txt" find-som-marker))
