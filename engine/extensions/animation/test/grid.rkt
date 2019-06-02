#lang racket 

(require "../main.rkt"
         2htdp/image
         )

;Kind of a performance benchmark.  It's got 64*6=384 entities, all changing size,rotation, transparency with some independence.  I'm getting about 20 FPS on my Chromebook, which seems reasonable, I think.

(define red   (register-sprite (square 48 'solid 'red)))
(define orange   (register-sprite (square 48 'solid 'orange)))
(define yellow   (register-sprite (square 48 'solid 'yellow)))
(define green (register-sprite (square 48 'solid 'green)))
(define blue   (register-sprite (square 48 'solid 'blue)))
(define purple   (register-sprite (square 48 'solid 'purple)))


(define-component clearer void?)


(define (grid n speed img)
  (define ticked #f)
  (define my-counter 0)
  (define my-rotation 0)
  (define my-transparency 0)

  (define (inc-counter!)
      (set! my-counter (+ my-counter
                          (* speed 0.01)))

      (set! my-rotation my-counter)
      (set! my-transparency (sin my-counter)))

  (list 
    (entity 
      (clearer #f 
               (inc-counter!)))

    (entity-grid 400 400 50
                 (thunk
                   (list 
                     (sprite img)      
                     (size (thunk* my-transparency))
                     (rotation (thunk* my-rotation))
                     (transparency (thunk* my-transparency))
                     )))))

(require "../hotswap.rkt")

(play!
  (let () 
    (game 
      (grid 'red 1 red)
      (grid 'orange 2 orange)
      (grid 'yellow 3 yellow)
      (grid 'green 5 green)
      (grid 'blue 7 blue)   
      (grid 'purple 11 purple))))





