#lang racket/base

(require racket/list
         racket/match
         racket/math)

(define instrs
  (call-with-input-file "day09.txt"
    (lambda (in)
      (for/list ([line (in-lines in)])
        (match-define (regexp #rx"(.) (.+)" (list _ dir (app string->number amt)))
          line)
        (cons (string-ref dir 0) amt)))))

(struct state (visited rope)
  #:transparent)

(define (display-state s [w 10] [h w])
  (match-define (state visited rope) s)
  (for ([r (in-range (- h) h)])
    (for ([c (in-range (- w) w)])
      (define p (cons r c))
      (cond
        [(index-of rope p) => display]
        [(hash-has-key? visited p) (display #\#)]
        [else (display #\.)]))
    (newline)))

(define (dir-u p) (cons (sub1 (car p)) (cdr p)))
(define (dir-d p) (cons (add1 (car p)) (cdr p)))
(define (dir-l p) (cons (car p) (sub1 (cdr p))))
(define (dir-r p) (cons (car p) (add1 (cdr p))))
(define (dist a b)
  (exact-round
   (sqrt
    (+ (sqr (- (car b) (car a)))
       (sqr (- (cdr b) (cdr a)))))))

(define (move dir p)
  ((case dir
     [(#\R) dir-r]
     [(#\L) dir-l]
     [(#\U) dir-u]
     [(#\D) dir-d])
   p))

(define (step-rope r dir)
  (let loop ([hd  (move dir (car r))]
             [tl  (cadr r)]
             [r   (cddr r)]
             [res null])
    (define next-dir
      (cond
        [(< (car hd) (car tl)) #\U]
        [(> (car hd) (car tl)) #\D]
        [(< (cdr hd) (cdr tl)) #\L]
        [(> (cdr hd) (cdr tl)) #\R]
        [else #f]))
    (define next-tl
      (cond
        [(<= (dist hd tl) 1) tl]
        [(or (= (car hd) (car tl))
             (= (cdr hd) (cdr tl)))
         (move next-dir tl)]
        [else
         (let ([tl (move next-dir tl)])
           (case next-dir
             [(#\L #\R) ((if (> (car hd) (car tl)) dir-d dir-u) tl)]
             [(#\U #\D) ((if (> (cdr hd) (cdr tl)) dir-r dir-l) tl)]))]))
    (if (null? r)
        (reverse (cons next-tl (cons hd res)))
        (loop next-tl (car r) (cdr r) (cons hd res)))))

(define (step s i)
  (match-define (cons dir amt) i)
  (for/fold ([s s])
            ([_ (in-range amt)])
    (match-define (state visited r) s)
    (define next-rope
      (step-rope r dir))
    (define next-visited
      (hash-set visited (last next-rope) #t))
    (state next-visited next-rope)))

(define (run s instrs)
  (hash-count
   (state-visited
    (for/fold ([s s])
              ([i (in-list instrs)])
      (define next-s
        (step s i))
      (begin0 next-s
        #;(display-state next-s 30 20)
        )))))

(define part1
  (run (state (hash) (make-list 2 (cons 0 0))) instrs))
(= part1 6284)

(define part2
  (run (state (hash) (make-list 10 (cons 0 0))) instrs))
(= part2 2661)
