#lang racket


(provide grid-of
         grid-map
         grid-map-with-index
         grid-get
         grid-set
         grid->quad
         list-getter)

(define (list-getter l)
  (lambda (x)
    (list-ref l x)))

(define (list-of t n)
  (map (thunk* t) (range n)))

(define (grid-of t n m)
  (define row (list-of t n))
  (list-of row m))



(define (grid-map f g)
  (define rf (curry map f))
  (map rf g))

(define (add-grid-indexes g)
  (for/list ([r (range (length g))])
    (for/list ([c (range (length (first g)))])
      (list (grid-get g r c) r c))))

(define (grid-map-with-index f g)
    (grid-map (curry apply f) (add-grid-indexes g)))


(define/contract (grid-get g r c)
  (-> (listof list?) number? number? any/c)
  (define r2 (max r 0))
  (define c2 (max c 0))
  (define r3 (min r2 (- (length g) 1)))
  (define c3 (min c2 (- (length (first g)) 1)))
  (list-ref (list-ref g r3) c3))

(define/contract (grid-set g r c v)
  (-> (listof list?) number? number? any/c (listof list?))
  (define row (list-ref g r))
  (define new-row (list-set row c v))
  (list-set g r new-row))

(define/contract (grid-get-adj g r c rd cd)
  (-> (listof list?) number? number? number? number? any/c)
  (grid-get g (+ r rd) (+ c cd)))

(define (tl-neighbor g r c)
  (grid-get-adj g r c -1 -1))

(define (t-neighbor g r c)
  (grid-get-adj g r c -1 0))

(define (l-neighbor g r c)
  (grid-get-adj g r c 0 -1))

(define (tr-neighbor g r c)
  (grid-get-adj g r c -1 1))

(define (r-neighbor g r c)
  (grid-get-adj g r c 0 1))

(define (br-neighbor g r c)
  (grid-get-adj g r c 1 1))

(define (b-neighbor g r c)
  (grid-get-adj g r c 1 0))

(define (bl-neighbor g r c)
  (grid-get-adj g r c 1 -1))


(define/contract (grid->quad g r c dir f)
  (-> (listof list?) number? number? symbol? (-> symbol? symbol?) (listof list?) )
  (match dir
    ['tl
     (list
      (list (tl-neighbor g r c) (t-neighbor g r c))
      (list (l-neighbor g r c) (f (grid-get g r c))))]
    ['tr
     (list
      (list (t-neighbor g r c) (tr-neighbor g r c))
      (list (f (grid-get g r c)) (r-neighbor g r c) ))]
    ['br
     (list
      (list (f (grid-get g r c)) (r-neighbor g r c))
      (list (b-neighbor g r c) (br-neighbor g r c) ))]
    ['bl
     (list
      (list (l-neighbor g r c) (f (grid-get g r c)) )
      (list (bl-neighbor g r c) (b-neighbor g r c) ))]))

