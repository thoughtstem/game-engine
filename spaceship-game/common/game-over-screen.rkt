#lang racket

(provide game-over-screen)

(require "../../game-engine.rkt")

(define (end-screen w h msg color)
  (new-sprite (list (overlay (text msg 30 color)
                             (rectangle w h "solid" (make-color 0 0 0 100))))
              1))

(define lose-screen (lambda (w h) (end-screen w h "GAME OVER!" "yellow")))
(define win-screen  (lambda (w h) (end-screen w h "YOU WIN!" "green")))

(define (game-over-screen won? lost?)
  (sprite->entity (square 1 "solid" (make-color 0 0 0 0))
                  #:position   (posn 0 0)
                  #:name       "ui"
                  #:components (hidden)
                               (every-tick (maybe-end won? lost?))))

(define (maybe-end won? lost?)
  (lambda (g e)
    (cond
      [(lost? g e) (show-end-screen g e (lose-screen (game-width g) (game-height g)))]
      [(won? g e)  (show-end-screen g e (win-screen  (game-width g) (game-height g)))]
      [else e])))

(define (show-end-screen g e sprite)
  (~> e
      (remove-component _ hidden?)
      (remove-component _ every-tick?)
      (update-entity _ posn? (posn (/ (game-width g) 2)
                                   (/ (game-height g) 2)))
      (update-entity _ animated-sprite? sprite)))