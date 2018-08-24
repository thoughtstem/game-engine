#lang racket

(require "../game-entities.rkt")
(require posn)

;(provide (struct-out on-edge))

(provide (rename-out (make-on-edge on-edge))
         on-edge?)

(struct on-edge (pos offset rule? func))

(define (make-on-edge pos #:rule [rule? (lambda (g e) #t)] #:offset [offset 0] func)
  (on-edge pos offset rule? func))

(define (update-on-edge g e c)
  (define WIDTH (game-width g))
  (define HEIGHT (game-height g))
  (define p (get-component e posn?))
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (define target-edge (on-edge-pos c))
  (define offset (on-edge-offset c))
  (if ((on-edge-rule? c) g e)
      (cond [(eq? target-edge 'left)   (if (<= pos-x (+ offset 0))      ((on-edge-func c) g e) e)]
            [(eq? target-edge 'right)  (if (>= pos-x (+ offset WIDTH))  ((on-edge-func c) g e) e)]
            [(eq? target-edge 'top)    (if (<= pos-y (+ offset 0))      ((on-edge-func c) g e) e)]
            [(eq? target-edge 'bottom) (if (>= pos-y (+ offset HEIGHT)) ((on-edge-func c) g e) e)])
      e))

(new-component on-edge?
               update-on-edge)
