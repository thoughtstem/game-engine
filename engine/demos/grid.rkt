#lang racket 

(require "../extensions/main.rkt"
         2htdp/image
         )

;Kind of a performance benchmark.  It's got 64*6=384 entities, all changing size,rotation, transparency with some independence.  I'm getting about 35 FPS on my Chromebook, which seems reasonable, I think.
;  It does make use of one optimization: Entities in a grid share memory that defines their rotation, transparency, etc.  They don't maintain this data separately.

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

    ;TODO: Can we mark these as "static" and skip them in the tick?
    ;  Hell yes we can!  Just wrap it in a component that renders it, but don't tick it.  Now it is ommited from the tick but retained as data.  The difference between code and data in lisp is the quote.  In this language, it's whether the component is inert or not -- has the same effect on the game tree that the quote has on the AST.  Game tree == AST in this metaphore
    (entity
      (also-render
        (game
          (entity-grid 400 400 50
                       (thunk*
                         (list 
                           (sprite img)      
                           (size (thunk* my-transparency))
                           (rotation (thunk* my-rotation))
                           (transparency (thunk* my-transparency)))))))
      
      )))


#;
(require "../extensions/hotswap.rkt")

(play!
  (game 
    (entity
      (position (posn 200 200))
      (counter 0 (add1 (get-counter)))
      (rotation 0 (sin (/ (get-counter) 10)))
      (sprite (register-sprite (triangle 50 'solid 'black))
              (if (= 100 (get-counter))
                ;Testing registration of sprite at runtime.  You'll notice a small hitch on the 100th step.
                (register-sprite (star 50 'solid 'black))
                (get-sprite))))
    (grid 'red 1 red)
    (grid 'orange 2 orange)
    (grid 'yellow 3 yellow)
    (grid 'green 5 green)
    (grid 'blue 7 blue)   
    (grid 'purple 11 purple)))





