#lang racket

(require "../main.rkt"
         "./util.rkt"
         2htdp/image)

;Abstract this...

(define outdoor-floor 
  (entity
    (position (posn 200 200))
    (sprite
      (register-sprite
        (square 400 'solid 'green)))))

(define indoor-floor
  (entity
    (position (posn 200 200))
    (sprite
      (register-sprite
        (square 400 'solid 'gray)))))


(define (outdoors)
  (game
    input-manager
    (blue-circle-avatar (posn 200 50))
    (door 
      (posn 200 10)
      door-open-close
      indoors)
    outdoor-floor))

(define (indoors)
  (game
    input-manager
    (blue-circle-avatar (posn 200 350))
    (door 
      (posn 200 390)
      door-open-close
      outdoors)
    indoor-floor))

(play! 
  (game
    (door-manager
      (outdoors))))









