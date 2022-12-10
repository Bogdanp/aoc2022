#lang racket/base

(require racket/match
         racket/port)

(define instrs
  (call-with-input-file "day10.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match line
          [(regexp #rx"addx (.+)" (list _ (app string->number n)))
           `(addx ,n)]
          ["noop"
           '(noop)])))))

(define (run instrs)
  (for/fold ([x 1] [states '(1)] #:result (reverse states))
            ([i (in-list instrs)])
    (match i
      [`(addx ,n)
       (define nx (+ x n))
       (values nx (list* nx x states))]
      [`(noop)
       (values x (cons x states))])))

(define (signal-strength instrs)
  (define states (list->vector (run instrs)))
  (for/sum ([c (in-range 20 (vector-length states) 40)])
    (* c (vector-ref states (sub1 c)))))

(define part1 (signal-strength instrs))
(= part1 15020)

(define (render instrs)
  (define screen (make-vector 241 #\.))
  (for ([(x i) (in-indexed (in-list (run instrs)))])
    (define p (modulo i 40))
    (when (and (>= p (sub1 x))
               (<= p (add1 x)))
      (vector-set! screen i #\#)))
  (call-with-output-string
   (lambda (out)
     (for ([i (in-range 6)])
       (for ([j (in-range 40)])
         (display (vector-ref screen (+ (* i 40) j)) out))
       (newline out)))))

(define part2 (render instrs))
(equal? part2 (string-append
               "####.####.#..#..##..#....###...##..###..\n"
               "#....#....#..#.#..#.#....#..#.#..#.#..#.\n"
               "###..###..#..#.#....#....#..#.#..#.#..#.\n"
               "#....#....#..#.#.##.#....###..####.###..\n"
               "#....#....#..#.#..#.#....#....#..#.#....\n"
               "####.#.....##...###.####.#....#..#.#....\n"))
