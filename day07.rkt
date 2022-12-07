#lang racket/base

(require racket/match
         racket/string)

(struct ent (name) #:transparent)
(struct filent ent (size) #:transparent)
(struct dirent ent (children) #:transparent)

(define (join a b)
  (if (string=? a "/")
      (string-append "/" b)
      (string-append a "/" b)))

(define (dirname a)
  (define parts (reverse (string-split a "/")))
  (string-append "/" (string-join (reverse (cdr parts)) "/")))

(define root
  (call-with-input-file "day07.txt"
    (lambda (in)
      (define ents
        (make-hash (list (cons "/" (dirent "/" null)))))
      (for/fold ([curr "/"])
                ([line (in-lines in)])
        (match line
          ["$ cd .." (dirname curr)]
          ["$ cd /" "/"]
          [(regexp #rx"\\$ cd (.+)" (list _ name))
           (join curr name)]
          [(regexp #rx"\\$ ls")
           (define children
             (let loop ([children null])
               (match (peek-char in)
                 [(or #\$ (? eof-object?))
                  (reverse children)]
                 [_ (match (read-line in)
                      [(regexp #rx"dir (.+)" (list _ name))
                       (define p (join curr name))
                       (define e (dirent p null))
                       (hash-set! ents p e)
                       (loop (cons p children))]
                      [(regexp #rx"(.+) (.+)" (list _ (app string->number size) name))
                       (define p (join curr name))
                       (define e (filent p size))
                       (hash-set! ents p e)
                       (loop (cons p children))])])))
           (begin0 curr
             (hash-update! ents curr (λ (e) (struct-copy dirent e [children children]))))]))
      (let loop ([e (hash-ref ents "/")])
        (match e
          [(dirent name children)
           (dirent name (map loop children))]
          [(? filent? e) e]
          [(? string?)
           (loop (hash-ref ents e))])))))

(define dirsize
  (let ([memo (make-hasheq)])
    (lambda (d)
      (hash-ref! memo d (λ ()
                          (let loop ([e d])
                            (match e
                              [(filent _ size) size]
                              [(dirent _ children)
                               (apply + (map loop children))])))))))

(define part1
  (let loop ([e root])
    (match e
      [(filent _ _) 0]
      [(dirent _ children)
       (define size (dirsize e))
       (apply + (if (<= size 100000) size 0) (map loop children))])))

(define part2
  (let ([need (- 30000000 (- 70000000 (dirsize root)))])
    (define candidates
      (let loop ([e root])
        (match e
          [(filent _ _ ) null]
          [(dirent _ children)
           (if (>= (dirsize e) need)
               (apply append (list e) (map loop children))
               null)])))
    (dirsize (car (sort candidates #:key dirsize <)))))
