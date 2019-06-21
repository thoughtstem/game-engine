#lang racket

(provide show-mouse-state
         point-to-mouse
         mouse-in-game?
         mouse-button-is-down?
         get-mouse-pos)

(require "../game-entities.rkt"
         "./movement-util.rkt"
         posn)

(define (mouse-in-game? g e)
  (define m-pos (get-mouse-pos g))
  (not (equal? m-pos (posn 0 0)))
  )

(define (get-mouse-pos g)
  (define raw-posn (get-raw-mouse-pos g))
  (define raw-x (posn-x raw-posn))
  (define raw-y (posn-y raw-posn))
  (if ml-scale-info
      (let ([scale-x  (first  (second ml-scale-info))]
            [scale-y  (second (second ml-scale-info))]
            [scale-w  (first  (third  ml-scale-info))]
            [scale-h  (second (third  ml-scale-info))]
            [window-w (first  (fifth  ml-scale-info))]
            [window-h (second (fifth  ml-scale-info))])
        (posn (/ (- raw-x (/ (- window-w scale-w) 2)) scale-x)
              (/ (- raw-y (/ (- window-h scale-h) 2)) scale-y)))
      raw-posn))

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
