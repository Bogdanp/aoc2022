#lang racket/base

(define trees
  (call-with-input-file "day08.txt"
    (lambda (in)
      (for*/hash ([(line row) (in-indexed (in-lines in))]
                  [(height col) (in-indexed (in-string line))])
        (values (cons row col) (- (char->integer height) 48))))))

(define (find-visible trees pos dir-proc)
  (let loop ([pos pos]
             [tallest (hash-ref trees pos)]
             [visible (list pos)])
    (define next-pos (dir-proc pos))
    (define next (hash-ref trees next-pos #f))
    (cond
      [(not next)
       (reverse visible)]
      [(> next tallest)
       (loop next-pos next (cons next-pos visible))]
      [else
       (loop next-pos tallest visible)])))

(define rowmax (apply max (map car (hash-keys trees))))
(define colmax (apply max (map cdr (hash-keys trees))))

(define (dir-n p) (cons (sub1 (car p)) (cdr p)))
(define (dir-s p) (cons (add1 (car p)) (cdr p)))
(define (dir-w p) (cons (car p) (sub1 (cdr p))))
(define (dir-e p) (cons (car p) (add1 (cdr p))))

(define part1
  (let ([visible (make-hash)])
    (for ([r (in-inclusive-range 0 rowmax)])
      (for ([pos (in-list (find-visible trees (cons r 0) dir-e))])
        (hash-set! visible pos #t))
      (for ([pos (in-list (find-visible trees (cons r colmax) dir-w))])
        (hash-set! visible pos #t)))
    (for ([c (in-inclusive-range 0 colmax)])
      (for ([pos (in-list (find-visible trees (cons 0 c) dir-s))])
        (hash-set! visible pos #t))
      (for ([pos (in-list (find-visible trees (cons rowmax c) dir-n))])
        (hash-set! visible pos #t)))
    (hash-count visible)))

(define (viewing-distance trees pos dir-proc)
  (define tree (hash-ref trees pos))
  (let loop ([pos pos]
             [dist 0])
    (define next-pos (dir-proc pos))
    (define next (hash-ref trees next-pos #f))
    (cond
      [(not next) dist]
      [(>= next tree) (add1 dist)]
      [else (loop next-pos (add1 dist))])))

(define part2
  (let ([scores (make-hash)])
    (for ([pos (in-hash-keys trees)])
      (define score
        (* (viewing-distance trees pos dir-n)
           (viewing-distance trees pos dir-s)
           (viewing-distance trees pos dir-e)
           (viewing-distance trees pos dir-w)))
      (hash-set! scores pos score))
    (cdar (sort (hash->list scores) #:key cdr >))))
