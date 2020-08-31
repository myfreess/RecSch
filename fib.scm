(add-to-load-path (dirname (current-filename)))

(use-modules (morphisms))
;; Nat a = 0 | (cons 1 a)


;;pred: a -> bool
(define (one? x) (and (integer? x) (= x 1)))
(define (zero? x) (and (integer? x) (= x 0)))

(define (non-z-nat? x) (and (pair? x) (one? (car x))))

(define (nat-map f n)
  (cond
   [(zero? n) n]
   [(non-z-nat? n)
    (cons 1 (f (cdr n)))]
   [else (error "Not a Natural Number")]))

(define (natural-CoAlg n)
  (cond
   [(zero? n) n]
   [else (cons 1 (- n 1))]))

(define (fibonacci-Cov-Alg m)
  (cond
   [(zero? m) m]
   [(zero? (elim-memo (cdr m))) 1]
   [else
    (let* ([r (cdr m)]
	   [n1 (elim-value r)]
	   [n2 (elim-value
		 (cdr (elim-memo r)))])
      (+ n1 n2))]))

(define (fibonacci-Alg x)
  (cond
   [(zero? x) (list x)]
   [(zero? (cadr x)) (list 1 0)]
   [else
    (let* ([r (cdr x)]
	   [n1 (car r)]
	   [n2 (cadr r)])
      (cons (+ n1 n2) r))]))

;;有意思的地方是，(Cofree Nat)与List同构
	     
    
