#lang racket

(require "../main.rkt"
         "./util.rkt"
         2htdp/image)

(define gray-floor
  (register-sprite
   (square 400 'solid 'gray)))

(define (tile-floor) 
  (entity
    (position (posn 200 200))
    (sprite gray-floor)))

(define (avatar-position old-coord coord)
  (match (posn-subtract old-coord coord)
    [(posn 1 0) (posn 50 200)] 
    [(posn -1 0) (posn 350 200)] 
    [(posn 0 -1) (posn 200 350)] 
    [(posn 0 1) (posn 200 50)]))

;Mother fucker, this is some cool recursive shit.
;Need to write a pl paper about this.
;   The player is literally embodied in the code.

;Also, by simulating a pacman universe, is our code topologically equivalent to a donut?
;   Could you have proved that property of your system in another language?
(define (tile old-coord coord)
  (game
    (input-manager)
    (blue-circle-avatar 
      (avatar-position old-coord coord))

    (door 
      (position (posn 200 0))
      door-open-close
      (lambda () (tile 
                   coord
                   (posn-add coord (posn 0 1)))))

    (door 
      (position (posn 200 400))
      door-open-close
      (lambda () (tile 
                   coord
                   (posn-add coord (posn 0 -1)))))

    (door 
      (position (posn 400 200))
      door-open-close
      (lambda () (tile 
                   coord
                   (posn-add coord (posn -1 0)))))

    (door 
      (position (posn 0 200))
      door-open-close
      (lambda () (tile 
                   coord
                   (posn-add coord (posn 1 0)))))


    (tile-floor)))


(play! 
  (game
    (door-manager
      (tile 
        (posn -1 0)
        (posn 0 0)))))









