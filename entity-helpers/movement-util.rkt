#lang racket

(provide randomly-relocate-me)
(provide go-to-random)
(provide go-to)
(provide go-to-pos)
(provide go-to-pos-inside)
(provide respawn)
(provide move-with-speed)
(provide move-random-speed)

(require "../game-entities.rkt")
(require "../components/direction.rkt")
(require "../components/every-tick.rkt")
(require "../ai.rkt")

(require posn)

(define (randomly-relocate-me min-x max-x min-y max-y)
  (lambda (g e)
    (update-entity e posn? (posn (random min-x max-x)
                                 (random min-y max-y)))))

(define (go-to-random min-x max-x min-y max-y)
  (lambda (g e)
    (update-entity e posn? (posn (random min-x (add1 max-x))
                                 (random min-y (add1 max-y))))))

(define (go-to pos-x pos-y)
  (lambda (g e)
    (update-entity e posn? (posn pos-x pos-y))))

(define (go-to-pos pos)
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define p (get-component e posn?))
    (define pos-x (posn-x p))
    (define pos-y (posn-y p))
    (update-entity e posn?
                   (cond
                     [(eq? pos 'left)         (posn 0     pos-y)]
                     [(eq? pos 'right)        (posn WIDTH pos-y)]
                     [(eq? pos 'top)          (posn pos-x 0)]
                     [(eq? pos 'bottom)       (posn pos-x HEIGHT)]
                     [(eq? pos 'top-left)     (posn 0     0)]
                     [(eq? pos 'top-right)    (posn WIDTH 0)]
                     [(eq? pos 'bottom-left)  (posn 0     HEIGHT)]
                     [(eq? pos 'bottom-right) (posn WIDTH HEIGHT)]
                     [(eq? pos 'left-center)  (posn 0     (/ HEIGHT 2))]
                     [(eq? pos 'right-center) (posn WIDTH (/ HEIGHT 2))]
                     [(eq? pos 'top-center)   (posn (/ WIDTH 2) 0)]
                     [(eq? pos 'bottom-center)(posn (/ WIDTH 2) HEIGHT)]))))
                         
(define (go-to-pos-inside pos)
  (lambda (g e)
    (define WIDTH (game-width g))
    (define HEIGHT (game-height g))
    (define p (get-component e posn?))
    (match-define (bb e-w e-h) (get-component e bb?))
    (define hw (+ (/ e-w 2) 2)) ;
    (define hh (+ (/ e-h 2) 2))
    (define pos-x (posn-x p))
    (define pos-y (posn-y p))
    (update-entity e posn?
                   (cond
                     [(eq? pos 'left)         (posn hw           pos-y)]
                     [(eq? pos 'right)        (posn (- WIDTH hw) pos-y)]
                     [(eq? pos 'top)          (posn pos-x        hh)]
                     [(eq? pos 'bottom)       (posn pos-x        (- HEIGHT hh))]
                     [(eq? pos 'top-left)     (posn hw           hh)]
                     [(eq? pos 'top-right)    (posn (- WIDTH hw) hh)]
                     [(eq? pos 'bottom-left)  (posn hw           (- HEIGHT hh))]
                     [(eq? pos 'bottom-right) (posn (- WIDTH hw) (- HEIGHT hh))]
                     [(eq? pos 'left-center)  (posn hw           (/ HEIGHT 2))]
                     [(eq? pos 'right-center) (posn (- WIDTH hw) (/ HEIGHT 2))]
                     [(eq? pos 'top-center)   (posn (/ WIDTH 2)  hh)]
                     [(eq? pos 'bottom-center)(posn (/ WIDTH 2)  (- HEIGHT hh))]))))    

(define (respawn edge)
  (lambda (g e)
    (define HEIGHT (game-height g))
    (define WIDTH (game-width g))
    ((cond
      [(eq? edge 'left)   (go-to 0 (random 0 HEIGHT))]
      [(eq? edge 'right)  (go-to WIDTH (random 0 HEIGHT))]
      [(eq? edge 'top)    (go-to (random 0 WIDTH) 0)]
      [(eq? edge 'bottom)  (go-to (random 0 WIDTH) HEIGHT)]
      [(eq? edge 'anywhere) (go-to (random 0 WIDTH) (random 0 HEIGHT))])
      g e)))

(define (move-with-speed spd)
  (lambda (g e)
    (define dir (get-direction e))
    (update-entity e every-tick?
                     (every-tick (move-dir-spd #:dir dir #:speed spd)))))

(define (move-random-speed min max)
  (lambda (g e)
    (define dir (get-direction e))
    (update-entity e every-tick?
                     (every-tick (move-dir-spd #:dir dir #:speed (random min (add1 max)))))))