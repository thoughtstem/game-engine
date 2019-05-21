#lang racket

(require "../main.rkt"
         "./util.rkt"
         2htdp/image) 

;Highly experimental...
;  Weird and broken.  It kinda tries to turn all of an entity's compents into entities that can be rendered -- thus making a visualization out of the structure of an entity.  It's a cool meta level to be at, which lets us create a lot of cogntive tools for visualizing the state of code at runtime.  Enables live programming and literally blurring the line between game and source code.  Your avatar can interact with both.

;TODO: Better model for dealing with subgames.  Where do they render?  DO the render smaller?  What is a subgame?  Any component whose value is a game?  Or only the component sub-game?  
;TODO: Text sprites for rendering component name in the visualization
;TODO: Default ways to visualize basic and compound racket data, if that's what the component is storing.
;TODO: Should probably work bottom up.  How does component render?  That quickly gets into, how does a game render?  Sub-game and also-render are meta components...

(define-component visualizing entity?)

(define (entity-visualizer e)
  (entity
    (visualizing e 
                 (^ tick-entity)) 

    (sub-game (entity->vizualizing-game 
                e)
              (entity->vizualizing-game
                (get-visualizing)))

    (also-render (game) 
                 (get-sub-game))))

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

(define blue-star
  (register-sprite
    (star 20 'solid 'blue)))


(define (component->vizualizing-entity c ci)
  (entity
    (also-render
      (game
        (entity
          (position (posn 50 (+ 50 (* 50 ci))))  
          (sprite red-circle))

        (entity
          (position (posn 100 (+ 50 (* 50 ci))))      
          (cond
            [(sprite? c) c]
            [(also-render? c) c]

            ;Idea: use physics to do constraint-based layouts instead of annoying math...
            ;Idea: use level manager to cycle through a level per entity, the z-direction...
            ;TODO: Not rendering.  When it does, can we translate and scale down the rendering based on the recursion depth?
            [else
              (sprite red-circle)]))))))

(begin
  (play!
    (game
      (entity-visualizer
        (entity
          (counter 0)
          (position (posn 0 0))
          (sprite green-square)
          (sub-game (game
                      (entity 
                        (name 'star-1)
                        (counter 0 (^ add1))
                        (position (posn 200 200))
                        (rotation 0 (sin (/ (get-counter) 100)))
                        (sprite blue-star)) 
                      (entity 
                        (name 'star-2)
                        (position (posn 200 250))
                        (sprite blue-star)))
                    (^ tick))
          (also-render (game) 
                       (get-sub-game)))))))

