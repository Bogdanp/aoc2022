#lang racket/base

(require data/heap)

(define-values (grid start end)
  (call-with-input-file "day12.txt"
    (lambda (in)
      (for*/fold ([grid (hash)] [start #f] [end #f])
                 ([(line r) (in-indexed (in-lines in))]
                  [(char c) (in-indexed (in-string line))])
        (define p (cons r c))
        (case char
          [(#\S)
           (values (hash-set grid p 0) p end)]
          [(#\E)
           (values (hash-set grid p 25) start p)]
          [else
           (values (hash-set grid p (- (char->integer char) 97)) start end)])))))

(define (rebuild-path from current)
  (let loop ([path (list current)]
             [current current])
    (define p (hash-ref from current #f))
    (if p (loop (cons p path) p) path)))

(define (candidates grid p)
  (define possible
    (list
     (cons (sub1 (car p)) (cdr p))
     (cons (add1 (car p)) (cdr p))
     (cons (car p) (sub1 (cdr p)))
     (cons (car p) (add1 (cdr p)))))
  (define e (add1 (hash-ref grid p)))
  (for*/list ([p* (in-list possible)]
              [e* (in-value (hash-ref grid p* +inf.0))]
              #:when (<= e* e))
    p*))

(define (a* grid start goal [h (λ (_) 1)])
  (define from (make-hash))
  (define gscore (make-hash `((,start . 0))))
  (define fscore (make-hash `((,start . ,(h start)))))
  (define open (make-heap (λ (a b) (< (hash-ref fscore a)
                                      (hash-ref fscore b)))))
  (heap-add! open start)
  (let loop ()
    (cond
      [(zero? (heap-count open)) #f]
      [else
       (define current
         (heap-min open))
       (cond
         [(equal? current goal)
          (rebuild-path from current)]
         [else
          (heap-remove! open current)
          (define ps (candidates grid current))
          (for ([p (in-list ps)])
            (define tg-score (+ (hash-ref gscore current +inf.0) 1))
            (when (< tg-score (hash-ref gscore p +inf.0))
              (hash-set! from p current)
              (hash-set! gscore p tg-score)
              (hash-set! fscore p (+ tg-score (h p)))
              (heap-add! open p)))
          (loop)])])))

(define part1
  (time
   (sub1 (length (a* grid start end)))))
(= part1 468)

(define part2
  (time
   (let ([paths (for*/list ([(p e) (in-hash grid)]
                            #:when (zero? e)
                            [path (in-value (a* grid p end))]
                            #:when path)
                  (sub1 (length path)))])
     (car (sort paths <)))))
(= part2 459)
