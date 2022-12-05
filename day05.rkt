#lang racket/base

(require racket/list
         racket/match
         threading)

(struct instr (n from to))

(define-values (stacks instrs)
  (call-with-input-file "day05.txt"
    (lambda (in)
      (define stacks
        (for/fold ([stacks (hasheq)]
                   #:result (for/hasheq ([(k v) (in-hash stacks)])
                              (values k (reverse v))))
                  ([line (in-lines in)])
          #:break (string=? line "")
          (define len (string-length line))
          (for/fold ([stacks stacks])
                    ([num (in-naturals 1)]
                     [idx (in-range 0 len 4)])
            (define str (substring line idx (min len (+ idx 3))))
            (cond
              [(regexp-match? #rx"\\[.\\]" str)
               (define c (string-ref str 1))
               (hash-update stacks num (λ (cs) (cons c cs)) null)]
              [else
               stacks]))))
      (define instrs
        (for/list ([line (in-lines in)])
          (match line
            [(regexp #rx"move (.+) from (.+) to (.+)"
                     (list _
                           (app string->number n)
                           (app string->number from)
                           (app string->number to)))
             (instr n from to)])))
      (values stacks instrs))))

(define (~res s)
  (define heads
    (for/list ([num (in-inclusive-range (apply min (hash-keys s))
                                        (apply max (hash-keys s)))])
      (car (hash-ref s num))))
  (apply string heads))

(define part1
  (~res
   (for/fold ([s stacks])
             ([i (in-list instrs)])
     (match-define (instr n from to) i)
     (for/fold ([s s])
               ([_ (in-range n)])
       (define from-st
         (hash-ref s from))
       (~> (hash-set s from (cdr from-st))
           (hash-set to (cons (car from-st)
                              (hash-ref s to null))))))))

(define part2
  (~res
   (for/fold ([s stacks])
             ([i (in-list instrs)])
     (match-define (instr n from to) i)
     (define from-st (hash-ref s from))
     (~> (hash-set s from (drop from-st n))
         (hash-set to (append (take from-st n)
                              (hash-ref s to null)))))))
