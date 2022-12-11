#lang racket/base

(require racket/match
         racket/string)

(struct monke (items op test then-target else-target inspections)
  #:transparent)

(define (parse-op s)
  (match-define (list lhs-s op-s rhs-s)
    (string-split s))
  (define (operand v-s old)
    (if (string=? v-s "old") old (string->number v-s)))
  (λ (old)
    (define lhs (operand lhs-s old))
    (define rhs (operand rhs-s old))
    (case op-s
      [("+") (+ lhs rhs)]
      [("*") (* lhs rhs)])))

(define monkeys
  (call-with-input-file "day11.txt"
    (lambda (in)
      (let loop ([monkeys (hasheqv)])
        (cond
          [(eof-object? (peek-byte in)) monkeys]
          [else
           (match-define (regexp #rx"Monkey ([^:]+):" (list _ (app string->number id)))
             (read-line in))
           (match-define (regexp #rx"Starting items: (.+)" (list _ items-str))
             (read-line in))
           (match-define (regexp #rx"Operation: new = (.+)" (list _ (app parse-op op)))
             (read-line in))
           (match-define (regexp #rx"Test: divisible by (.+)" (list _ (app string->number test)))
             (read-line in))
           (match-define (regexp #rx"If true: throw to monkey (.+)" (list _ (app string->number then-target-id)))
             (read-line in))
           (match-define (regexp #rx"If false: throw to monkey (.+)" (list _ (app string->number else-target-id)))
             (read-line in))
           (void (read-line in))
           (define items
             (map string->number (string-split items-str ", ")))
           (define m
             (monke items op test then-target-id else-target-id 0))
           (loop (hash-set monkeys id m))])))))

(define (turn m relief-proc)
  (match-define (monke items op test then-target else-target _) m)
  (for/fold ([targets (hasheqv)]
             #:result (for/hasheqv ([(k v) (in-hash targets)])
                        (values k (reverse v))))
            ([item (in-list items)])
    (define worry-level
      (relief-proc (op item)))
    (hash-update targets
                 (if (zero? (modulo worry-level test))
                     then-target
                     else-target)
                 (λ (items)
                   (cons worry-level items))
                 null)))

(define ((make-item-adder items) m)
  (struct-copy monke m [items (append (monke-items m) items)]))

(define (run-round ms relief-proc)
  (define ids (sort (hash-keys ms) <))
  (for/fold ([ms ms])
            ([id (in-list ids)])
    (define m (hash-ref ms id))
    (define new-m
      (struct-copy monke m
                   [items null]
                   [inspections (+ (monke-inspections m)
                                   (length (monke-items m)))]))
    (for/fold ([ms (hash-set ms id new-m)])
              ([(id items) (in-hash (turn m relief-proc))])
      (hash-update ms id (make-item-adder items)))))

(define (solution ms n-rounds [relief-proc values])
  (define evaled-ms
    (for/fold ([ms ms])
              ([_ (in-range n-rounds)])
      (run-round ms relief-proc)))
  (define sorted-ms
    (sort (map cdr (hash->list evaled-ms)) #:key monke-inspections >))
  (* (monke-inspections (car sorted-ms))
     (monke-inspections (cadr sorted-ms))))

(define part1
  (solution monkeys 20 (λ (v) (quotient v 3))))
(= part1 61005)

(define the-lcm
  (apply lcm (map monke-test (hash-values monkeys))))
(define part2
  (solution monkeys 10000 (λ (v) (modulo v the-lcm))))
(= part2 20567144694)
