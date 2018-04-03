#lang racket
(require posn)

(define (posn-dot posn-a posn-b)
  (match-define
    (list px py)
    (list (- (posn-x posn-b) (posn-x posn-a))
          (- (posn-y posn-b) (posn-y posn-a))))
  (+ (* px px) (* py py)))

(define (perpendicular-point point-a point-b point-p)
  (match-define
    (list (posn x1 y1)
          (posn x2 y2)
          (posn x3 y3))
    (list point-a
          point-b
          point-p))
  (match-define
    (list px py)
    (list (- x2 x1) (- y2 y1)))
  (define dot-ab (posn-dot point-a point-b))
  (define x-part (* (- x3 x1) px))
  (define y-part (* (- y3 y1) py))
  (define u (/ (+ x-part y-part) dot-ab))
  (define x (+ x1 (* px u)))
  (define y (+ y1 (* py u)))
  (posn x y))

(define (posn-in-rect? lp-p lp-a lp-b)
  (and (n-between? (posn-x lp-p) (posn-x lp-a) (posn-x lp-b))
       (n-between? (posn-y lp-p) (posn-y lp-a) (posn-y lp-b))))

(define (n-between? n na nb)
  (< (n-min na nb) n (n-max na nb)))

(define (n-max n1 n2)
  (if (> n1 n2) n1 n2))

(define (n-min n1 n2)
  (if (< n1 n2) n1 n2))

(define (sqr-mag pos)
  (+ (* (posn-x pos) (posn-x pos))
     (* (posn-y pos) (posn-y pos))))

(define (mag pos)
  (sqrt (sqr-mag pos)))

(define (posn-diff point-a point-b)
  (posn (- (posn-x point-a) (posn-x point-b))
        (- (posn-y point-a) (posn-y point-b))))

(define (posn-add point-a point-b)
  (posn (+ (posn-x point-a) (posn-x point-b))
        (+ (posn-y point-a) (posn-y point-b))))

(define (circle-hits-line? circle-p circle-radius point-a point-b)
  (define perp-point (perpendicular-point point-a point-b circle-p))
  (or (> circle-radius (mag (posn-diff circle-p point-a)))
    (> circle-radius (mag (posn-diff circle-p point-b)))
    (and (posn-in-rect? perp-point point-a point-b)
       (< mag (posn-diff circle-p perp-point))
          circle-radius)))
  

(define (circle-hits-rect? circle-p circle-radius point-a point-b point-c point-d)
  ;(displayln "circle hits rect?\n")
  (or (posn-in-rect? circle-p point-a point-c)
      (circle-hits-line? circle-p circle-radius point-a point-b)
      (circle-hits-line? circle-p circle-radius point-b point-c)
      (circle-hits-line? circle-p circle-radius point-c point-d)
      (circle-hits-line? circle-p circle-radius point-d point-a)))


(define (rect-hits-rect? r1-p r1-w r1-h r2-p r2-w r2-h)
  ;(displayln "rect hits rect?\n")
  (match-define (posn e1-x e1-y) r1-p)
  (match-define (posn e2-x e2-y) r2-p)

  (define overlap 4)
  
  (define pad (if (and (<= overlap (/ r1-w 2))
                       (<= overlap (/ r1-h 2))
                       (<= overlap (/ r2-w 2))
                       (<= overlap (/ r2-h 2)))
                  overlap
                  0))

  (if (and (>= (- e1-x e2-x) (- (- (+ (/ r1-w 2) (/ r2-w 2)) pad)))
           (<= (- e1-x e2-x)    (- (+ (/ r1-w 2) (/ r2-w 2)) pad))
           (>= (- e1-y e2-y) (- (- (+ (/ r1-h 2) (/ r2-h 2)) pad)))
           (<= (- e1-y e2-y)    (- (+ (/ r1-h 2) (/ r2-h 2)) pad)))
      #t
      #f))


(define (circle-hits-circle? circle-p circle-radius circle-p2 circle-radius2)
  ;(displayln "circle hits circle?\n")
  (define directions (posn-diff circle-p circle-p2))
  (define proximity-max (+ circle-radius circle-radius2))
  (< (sqr-mag directions) (* proximity-max proximity-max)))

(provide rect-hits-rect?
         circle-hits-rect?
         circle-hits-circle?)

 ;(circle-hits-line? (posn 0 0) 1 (posn 1 1) (posn 3 1))

