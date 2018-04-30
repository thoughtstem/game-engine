#lang racket

(require "../game-entities.rkt")
(require "../entity-helpers/movement-util.rkt")
(require posn)

;(provide (struct-out wrap-around))
(provide (rename-out (make-wrap-around wrap-around)))

(struct wrap-around (mode))

(define (make-wrap-around [mode 'all-edges])
  (wrap-around mode))

(define (update-wrap-around g e c)
  (define WIDTH (game-width g))
  (define HEIGHT (game-height g))
  (define p (get-component e posn?))
  (define pos-x (posn-x p))
  (define pos-y (posn-y p))
  (define mode (wrap-around-mode c))
  (cond [(eq? mode 'all-edges)
         (cond [(< pos-x 0)      (update-entity e posn? (posn WIDTH     pos-y))]
               [(> pos-x WIDTH)  (update-entity e posn? (posn 0 pos-y))]
               [(< pos-y 0)      (update-entity e posn? (posn pos-x HEIGHT))]
               [(> pos-y HEIGHT) (update-entity e posn? (posn pos-x 0))]
               [else e])]
        [(eq? mode 'left-right)
         (cond [(< pos-x 0)      (update-entity e posn? (posn WIDTH     pos-y))]
               [(> pos-x WIDTH)  (update-entity e posn? (posn 0 pos-y))]
               [else e])]
        [(eq? mode 'top-bottom)
         (cond [(< pos-y 0)      (update-entity e posn? (posn pos-x HEIGHT))]
               [(> pos-y HEIGHT) (update-entity e posn? (posn pos-x 0))]
               [else e])]))

(new-component wrap-around?
               update-wrap-around)
