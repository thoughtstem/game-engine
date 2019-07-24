#lang racket

(provide conway-tick
	 square
	 overlay
	 above
	 beside
	 width
	 height
         alive?
	 conway-list->vector)


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
  (define ps2 (expand-width-to s2 s1))

  (append ps1 ps2))

(define (beside s1 s2)
  (define ps1 (expand-height-to s1 s2))
  (define ps2 (expand-height-to s2 s1))

  (map append ps1 ps2))


(define (safe-vector-ref l i)
  (cond 
    [(or (not (vector? l))
	 (< i 0) 
	 (>= i (vector-length l)) )
     #f]
    [else (vector-ref l i)]))

(define (alive? s x y)
  (eq? '*
       (safe-vector-ref 
	 (safe-vector-ref s y)
	 x)))

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

(define (die! s x y (n #f))
  (define v (if n n '_))
  (define row (vector-ref s y))
  (vector-set! row x v))


(define (live! s x y (n #f))
  (define v (if n n '*))
  (define row (vector-ref s y))
  (vector-set! row x v))


(define (conway-list->vector s)
  (list->vector (map list->vector s)))

(define (conway-tick s)
  (define last-s #f)
  (when (list? s)
	(set! s      (conway-list->vector s)))
  
  (set! last-s (vector-copy (vector-map vector-copy s)))

  

  (for ([r s]
	[y (in-naturals)])

    (for ([c r]
	  [x (in-naturals)])
      (define n (live-neighbors last-s x y))
      (define alive? (eq? c '*))

      (cond 
	[(and alive?  (< n 2))
	 (die! s x y )]
	[(and alive?  (or (= n 2) (= n 3)))
	 (live! s x y )]
	[(and alive?  (> n 3))
	 (die! s x y )]
	[(and (not alive?) (= n 3))
	 (live! s x y)])))

  s)

(define (square n)
  (define row (map (const '_) (range n)))

  (map (const row) (range n)))


(define (conway-vector->list v)
  (vector->list (vector-map vector->list v)))



(require rackunit) 

(test-case "conway impl"

	   (define pd
	     (overlay  
	       (square 5)
	       '((* * *)
		 (* _ *)
		 (* * *))))

	   (define pd2
	     (beside pd pd))

	   (define pd3
	     (above pd2 pd))

           (define pdv (conway-list->vector pd ))

	   (check-true
	     (alive? pdv 1 1))

	   (check-true
	     (alive? pdv 1 2))

	   (check-true
	     (alive? pdv 2 1))

	   (check-false
	     (alive? pdv -1 0))

	   (check-false
	     (alive? pdv 0 -1))

	   (check-equal?
	     (live-neighbors pdv 1 1)
	     2)

	   (check-equal? 
	     pd3
	     '((_ _ _ _ _ _ _ _ _ _)
	       (_ * * * _ _ * * * _)
	       (_ * _ * _ _ * _ * _)
	       (_ * * * _ _ * * * _)
	       (_ _ _ _ _ _ _ _ _ _)
	       (_ _ _ _ _ _ _ _ _ _)
	       (_ _ _ * * * _ _ _ _)
	       (_ _ _ * _ * _ _ _ _)
	       (_ _ _ * * * _ _ _ _)
	       (_ _ _ _ _ _ _ _ _ _)))

	   (check-equal? 
	     (conway-vector->list (conway-tick pd3))
	     '((_ _ * _ _ _ _ * _ _)
	       (_ * _ * _ _ * _ * _)
	       (* _ _ _ * * _ _ _ *)
	       (_ * _ * _ _ * _ * _)
	       (_ _ * _ _ _ _ * _ _)
	       (_ _ _ _ * _ _ _ _ _)
	       (_ _ _ * _ * _ _ _ _)
	       (_ _ * _ _ _ * _ _ _)
	       (_ _ _ * _ * _ _ _ _)
	       (_ _ _ _ * _ _ _ _ _)))


	   (check-equal?
	     (conway-vector->list
	       (conway-tick
		 (conway-tick pd3)))
	     '((_ _ * _ _ _ _ * _ _)
	       (_ * * * * * * * * _)
	       (* * _ * * * * _ * *)
	       (_ * * * * * * * * _)
	       (_ _ * * _ _ _ * _ _)
	       (_ _ _ * * _ _ _ _ _)
	       (_ _ _ * * * _ _ _ _)
	       (_ _ * * _ * * _ _ _)
	       (_ _ _ * * * _ _ _ _)
	       (_ _ _ _ * _ _ _ _ _)))


	   (check-equal?
	     (conway-vector->list
	       (conway-tick
		 (conway-tick
		   (conway-tick pd3))))
	     '((_ * * _ * * _ * * _)
	       (* _ _ _ _ _ _ _ _ *)
	       (* _ _ _ _ _ _ _ _ *)
	       (* _ _ _ _ _ _ _ _ *)
	       (_ * _ _ _ _ _ * * _)
	       (_ _ _ _ _ * _ _ _ _)
	       (_ _ _ _ _ _ * _ _ _)
	       (_ _ * _ _ _ * _ _ _)
	       (_ _ * _ _ _ * _ _ _)
	       (_ _ _ * * * _ _ _ _))) 

	   )
