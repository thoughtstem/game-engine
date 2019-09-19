#lang racket

(provide show-mouse-state
         point-to-mouse
         mouse-in-game?
         mouse-button-is-down?
         get-mouse-pos
         CROSSHAIR-IMG
         POINTER-IMG
         go-to-mouse
         touching-pointer?
         on-sprite-click
         )

(require "../game-entities.rkt"
         "./movement-util.rkt"
         "../components/on-mouse.rkt"
         posn
         2htdp/image
         )

(define (mouse-in-game? g e)
  (define m-pos (get-mouse-pos g))
  (not (equal? m-pos (posn 0 0)))
  )

(define (get-mouse-pos g)
  (define get-backing-scale (dynamic-require 'racket/gui/base 'get-display-backing-scale))
  (define display-scale (get-backing-scale))
  
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
        (posn (/ (- (* raw-x display-scale) (/ (- window-w scale-w) 2)) scale-x display-scale)
              (/ (- (* raw-y display-scale) (/ (- window-h scale-h) 2)) scale-y display-scale)
              ))
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

(define CROSSHAIR-IMG (overlay (line 0 26 'red)
                               (line 26 0 'red)
                               (circle 10 'outline 'red)))

(define POINTER-IMG (overlay (circle 2 'solid 'blue)
                             (circle 8 'outline 'yellow)
                             (circle 10 'outline 'red)
                             (square 24 'solid 'transparent)))

(define (go-to-mouse g e)
  (update-entity e posn? (get-mouse-pos g)))

(define (touching-pointer? g e)
  (define pos (get-posn e))
  (define m-pos (get-mouse-pos g))
  (define x (posn-x pos))
  (define y (posn-y pos))
  (define w (bb-w (get-component e bb?)))
  (define h (bb-h (get-component e bb?)))
  (define mx (posn-x m-pos))
  (define my (posn-y m-pos))
  (and (> mx (- x (/ w 2)))
       (< mx (+ x (/ w 2)))
       (> my (- y (/ h 2)))
       (< my (+ y (/ h 2)))))

(define (on-sprite-click #:key [key 'left] func)
  (on-mouse key #:rule touching-pointer? func))
