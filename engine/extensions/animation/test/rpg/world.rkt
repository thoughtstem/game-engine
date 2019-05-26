#lang racket

(provide world world-edge-system get-tile-world-coord)

(require "../../main.rkt"
         ;"./images.rkt"
         "./animations.rkt"
         2htdp/image)


(define (world bg-sprite)
  (entity
    (position (posn 0 0))

    ;TODO: Make animation helpers for this kind of thing.  But do we really want this effect in this game anyway?
    (transparency 0.5
                  (if (> (get-transparency) 0.99)
                    1
                    (+ 0.01 (get-transparency))))

    (sprite bg-sprite)))

(define-component world-coord posn?)

(define (get-tile-world-coord g)
  (get-world-coord (get-entity g
                               (name=? 'world-coordinate))))

(define (set-tile-world-coord g p)
  (update-entity g
                 (name=? 'world-coordinate)
                 (lambda (e)
                   (set-world-coord e p))))

(define (adjust-world-coord g adj-posn)
  (define new-coord
    (posn-add (get-tile-world-coord (CURRENT-GAME))
              adj-posn))
   
  (set-tile-world-coord g new-coord))

(define edge (register-sprite (rectangle 400 10 'solid 'red)))
(define (world-edge-system 
          #:to game-f
          #:transition (transition identity))

  ;TODO: Abstract these magic size and position numbers into parameters
  (list
    (entity
      (name 'world-coordinate)
      (world-coord (posn 0 0)))

    (door 
      #:to (thunk*
             (transition
               (adjust-world-coord
                 (game-f)
                 (posn 0 -1))))

      #:detect (thunk* 
                 (get-physics-colliding?
                   (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;TODO: Static?
                      #:static #t
                      )
      (position (posn 200 -10))
      (sprite edge)
      (rotation 0))

    (door 
      #:to (thunk*
             (transition
               (adjust-world-coord
                 (game-f)
                 (posn 1 0))))
      #:detect 
      (thunk* 
        (get-physics-colliding?
          (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;Static?
                      #:static #t
                      )
      (position (posn 410 200))
      (sprite edge)
      (rotation (/ pi 2)))

    (door 
      #:to (thunk*
             (transition
               (adjust-world-coord
                 (game-f)
                 (posn 0 1))))
      #:detect 
      (thunk* 
        (get-physics-colliding?
          (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;Static?
                      #:static #t
                      )
      (position (posn 200 410))
      (sprite edge)
      (rotation 0))

    (door 
      #:to (thunk*
             (transition
               (adjust-world-coord
                 (game-f)
                 (posn -1 0))))
      #:detect 
      (thunk* 
        (get-physics-colliding?
          (name=? 'avatar)))
      (physics-system 400 10
                      ;#:mass 1000000 ;Static?
                      #:static #t
                      )
      (position (posn -10 200))
      (sprite edge)
      (rotation (/ pi 2)))))



