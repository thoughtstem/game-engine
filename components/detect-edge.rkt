#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (struct-out detect-edge))

(struct detect-edge (name pos func))

(define (update-detect-edge g e c)
  (define WIDTH (game-width g))
  (define HEIGHT (game-height g))
  (define p (get-component (get-entity (detect-edge-name c) g) posn?))
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (define target-edge (detect-edge-pos c))
  (cond [(eq? target-edge 'left)   (if (<= pos-x 0)      ((detect-edge-func c) g e) e)]
        [(eq? target-edge 'right)  (if (>= pos-x WIDTH)  ((detect-edge-func c) g e) e)]
        [(eq? target-edge 'top)    (if (<= pos-y 0)      ((detect-edge-func c) g e) e)]
        [(eq? target-edge 'bottom) (if (>= pos-y HEIGHT) ((detect-edge-func c) g e) e)]))

(new-component detect-edge?
               update-detect-edge)
