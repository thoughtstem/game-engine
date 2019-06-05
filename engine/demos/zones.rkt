#lang racket

(require game-engine
         "./util.rkt"
         2htdp/image)

;Another game swapping example.  It may not look visually interesting, but you could theoretically stitch together an infinite quilt of games with this abstraction.

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

(define (tile old-coord coord)
  (game
    (input-manager)
    (blue-circle-avatar 
      (avatar-position old-coord coord))

    (door 
      #:to
      (thunk* (tile 
                coord
                (posn-add coord (posn 0 1))))
      (position (posn 200 0))
      door-open-close)

    (door 
      #:to
      (thunk* (tile 
                coord
                (posn-add coord (posn 0 -1))))
      (position (posn 200 400))
      door-open-close)

    (door 
      #:to
      (thunk* (tile 
                coord
                (posn-add coord (posn -1 0))))
      (position (posn 400 200))
      door-open-close)

    (door 
      #:to
      (thunk* (tile 
                coord
                (posn-add coord (posn 1 0))))
      (position (posn 0 200))
      door-open-close)


    (tile-floor)))


(play! 
  (game
    (door-manager
      (tile 
        (posn -1 0)
        (posn 0 0)))))









