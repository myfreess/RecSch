(define-module (morphisms)
  #:export
  (elim-value elim-memo fst snd :*:
   $mapRec cataT anaT hyloT histoT))


(use-modules (srfi srfi-9))

(define-record-type <product>
  (:*: e1 e2)
  product?
  (e1 fst)
  (e2 snd))

(define-record-type <Attr>
  (make-attr value memo)
  attr?
  (value elim-value)
  (memo elim-memo))



(define $mapRec)


(define (histoT h)
  (letrec ([w (λ(x)
                (let ([val ($mapRec w x)])
                  (make-attr (h val) val)))])
    (λ(e) (elim-value (w e)))))

(define (cataT alg)
  (λ(x)
    (alg ($mapRec (cataT alg) x))))

(define (hyloT alg coalg)
  (λ(x)
    (alg ($mapRec (hyloT alg coalg) (coalg x)))))

(define (anaT coalg)
  (λ(x) ($mapRec (anaT coalg) (coalg x))))



