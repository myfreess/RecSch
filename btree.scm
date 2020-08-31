
(add-to-load-path (dirname (current-filename)))

(use-modules (morphisms))

(use-modules (srfi srfi-9))

(define empty '())
(define (empty? x) (eq? x empty))

(define-record-type <btree>
  (make-btree val left right)
  btree?
  (val elim-content)
  (left elim-left)
  (right elim-right))

(define (binmap f x)
  (cond
   [(empty? x) x]
   [(btree? x)
    (make-btree
     (elim-content x)
     (f (elim-left x))
     (f (elim-right x)))]))

(define (heightAlg x)
  (cond
   [(empty? x) 0]
   [(btree? x)
    (1+
     (max
      (elim-left x)
      (elim-right x)))]))

(define get-bst-height
  (begin
    (set! $mapRec binmap)
     (cataT heightAlg)))
