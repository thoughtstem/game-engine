#lang racket

(provide show-mouse-state
         point-to-mouse
         mouse-in-game?
         mouse-button-is-down?)

(require "../game-entities.rkt"
         "./movement-util.rkt"
         posn)

(define (mouse-in-game? g e)
  (define m-pos (get-mouse-pos g))
  (not (equal? m-pos (posn 0 0))))

(define (show-mouse-state g e)
  (define m-pos (get-mouse-pos g))
  (define mouse-x (posn-x m-pos))
  (define mouse-y (posn-y m-pos))
  (displayln (~a "SHOWING MOUSE STATE: " mouse-x " " mouse-y))
  e)

(define (point-to-mouse g e)
  (define target-pos (get-mouse-pos g))
  ((point-to-posn target-pos) g e)
  )

(define (mouse-button-is-down? button)
  (lambda (g e)
    (mouse-button-down? button g)))