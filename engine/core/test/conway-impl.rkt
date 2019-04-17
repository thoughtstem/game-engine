#lang racket

(provide conway-tick
         square
         overlay
         above
         beside
         width
         height)


(define (pad-height g n)
  (define _n/2 (floor (/ n 2)))
  (define n/2  (ceiling (/ n 2)))

  (define pad-row
    (map (const '_) (range (width g))))

  (define padding-top
    (map (const pad-row) (range _n/2)))

  (define padding-bottom
    (map (const pad-row) (range n/2)))

  (append padding-top
          g
          padding-bottom))

(define (pad-width g n)
  (define _n/2 (floor (/ n 2)))
  (define n/2  (ceiling (/ n 2)))

  (define padding-left
    (map (const '_) (range _n/2))) 

  (define padding-right
    (map (const '_) (range n/2))) 

  (define (pad-row r)
    (append padding-left
            r
            padding-right))

  (map pad-row g))

(define (height g)
  (length g))

(define (width g)
  (length (first g)))

(define (expand-height-to g1 g2)
  (define diff
    (- (height g2) (height g1)))

  (if (positive? diff) 
    (pad-height g1 diff)
    g1) )

(define (expand-width-to g1 g2)
  (define diff
    (- (width g2) (width g1)))

  (if (positive? diff) 
    (pad-width g1 diff)
    g1))

(define (expand-to g1 g2)
  (define g1-ew  (expand-width-to g1 g2)) 
  (define g1-ewh (expand-height-to g1-ew g2)) 
  g1-ewh)

(define (overlay-symbol s1 s2)
  (if (eq? s1 '_)
    s2
    s1))

(define (overlay-row r1 r2)
  (map overlay-symbol r1 r2))

(define (overlay s1 s2)
  (define ps1 (expand-to s1 s2))
  (define ps2 (expand-to s2 s1))
  (map overlay-row ps1 ps2)   )

(define (above s1 s2)
  (define ps1 (expand-width-to s1 s2))

  (append ps1 s2))

(define (beside s1 s2)
  (define ps1 (expand-height-to s1 s2))
  (map append ps1 s2))


(define (safe-list-ref l i)
  (cond 
    [(or (not (list? l))
         (< i 0) 
         (>= i (length l)) )
     #f]
    [else (list-ref l i)]))

(define (alive? s x y)
  (eq? '*
       (safe-list-ref 
         (safe-list-ref s x)
         y)))

(define (live-neighbors s x y)
  (define north      (alive? s x       (- y 1)))  
  (define north-east (alive? s (+ x 1) (- y 1)))  
  (define east       (alive? s (+ x 1) y))
  (define south-east (alive? s (+ x 1) (+ y 1)))  
  (define south      (alive? s x       (+ y 1)))  
  (define south-west (alive? s (- x 1) (+ y 1)))  
  (define west       (alive? s (- x 1) y))
  (define north-west (alive? s (- x 1) (- y 1)))
  
  (length
    (filter identity
            (list north
                  north-east
                  east
                  south-east
                  south
                  south-west
                  west
                  north-west))))

(define (die s x y (n #f))
  (define v (if n n '_))
  (list-set s y
            (list-set (list-ref s y) x v)))

(define (live s x y (n #f))
  (define v (if n n '*))
  (list-set s y
            (list-set (list-ref s y) x v)))

(define (conway-tick s)
  (define new-s s)
  (for ([r s]
        [y (in-naturals)])

    (for ([c r]
          [x (in-naturals)])
      (define n (live-neighbors s x y))
      (define alive? (eq? c '*))

      (cond 
        [(and alive?  (< n 2))
         (set! new-s (die new-s x y))]
        [(and alive?  (or (= n 2) (= n 3)))
         (set! new-s (live new-s x y))]
        [(and alive?  (> n 3))
         (set! new-s (die new-s x y))]
        [(and (not alive?) (= n 3))
         (set! new-s (live new-s x y))])))
  
  new-s)

(define (square n)
  (define row (map (const '_) (range n)))

  (map (const row) (range n)))



