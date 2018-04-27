#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out on-edge))

(struct on-edge (pos func))

(define (update-on-edge g e c)
  (define WIDTH (game-width g))
  (define HEIGHT (game-height g))
  (define p (get-component e posn?))
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (define target-edge (on-edge-pos c))
  (cond [(eq? target-edge 'left)   (if (<= pos-x 0)      ((on-edge-func c) g e) e)]
        [(eq? target-edge 'right)  (if (>= pos-x WIDTH)  ((on-edge-func c) g e) e)]
        [(eq? target-edge 'top)    (if (<= pos-y 0)      ((on-edge-func c) g e) e)]
        [(eq? target-edge 'bottom) (if (>= pos-y HEIGHT) ((on-edge-func c) g e) e)]))

(new-component on-edge?
               update-on-edge)
