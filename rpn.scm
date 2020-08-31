(add-to-load-path (dirname (current-filename)))

(use-modules (morphisms))

(define op? procedure?)

(define (parseT l)
  (cond
   [(null? l) l]
   [(equal? (car l) "+") (cons + (cdr l))]
   [(equal? (car l) "*") (cons * (cdr l))]
   [else (cons (string->number (car l)) (cdr l))]))

(define (genT prel)
  (if (null? prel)
      identity
      (let ([k (cdr prel)]
            [e (car prel)])
        (cond
         [(number? e)
          (λ(s) (k (cons e s)))]
         [(op? e)
          (λ(s) (k (cons (e (car s) (cadr s)) (cddr s))))]))))


;; only support + and *
;; but it can beyond rig
(define (calculator expr)
  (((hyloT genT parseT) (string-split expr #\space)) '()))

(define (print-stack l)
  (let loop ([s l])
  (if (null? s)
      (display "|> ")
      (let ([e (car s)])
        (loop (cdr s))
        (display e)
        (display #\space))))
  (newline))
