#lang racket

(require "../main.rkt"
         "./util.rkt"
         2htdp/image) 
;Highly experimental...

(define-component vizualizing entity?)

(define (entity-visualizer e)
  (entity
    (vizualizing e 
                 #;
                 (^ tick-entity)) 

    (sub-game (entity->vizualizing-game e))

    (also-render (game) (get-sub-game))))

(define (entity->vizualizing-game e)
  (define cs (entity-components e))   

  (apply game
    (map component->vizualizing-entity cs (range (length cs)))))

(define red-circle
  (register-sprite
    (circle 20 'solid 'red)))

(define green-square
  (register-sprite
    (square 20 'solid 'green)))


(define (component->vizualizing-entity c ci)
  (entity
    (also-render
      (game
        (entity
          (sprite red-circle) 
          (position (posn 50 (+ 50 (* 50 ci)))))
        (entity
          (cond
            [(sprite? c) c]
            ;What if it's a sub-game?  Or contains a game? ?
            ;What if it contains an entity? (e.g. a system)
            ;Want to recursively render...
            ;Idea: use physics to do constraint-based layouts instead of annoying math...
            ;Idea: use level manager to cycle through a level per entity, the z-direction...
            [else
              (sprite red-circle)]) 
          (position (posn 100 (+ 50 (* 50 ci)))))))))

(play!
  (game
    (entity-visualizer
      (entity
        (counter 0)
        (position (posn 0 0))
        (sprite green-square)))))

