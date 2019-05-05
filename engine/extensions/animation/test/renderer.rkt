#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image))


;TODO: Maybe a few more examples to get the feel for crafting logic with signals.  
;  -> Add input?? 

;TODO: Make conway fast again.  Query optimizations.  Caching.

;Keep having ideas about using rosette or constraint based programming to do
; * Procedural geneartion
; * Generating entire games, sequences
; * Flockin behavior
;Or other meta stuff:
; * A component containing a game and a behaviour that ticks it...
; * Run subgames within a game...
; * Procedurally create games at runtime, run them, do something with the result.
; * "Bake" a game, by running it and observing its values.  Faster now as a sub-game...
;For the paper we write about this engine:
; * Game-oriented programming?
; * A game-based programming "paradigmn"?


;WHyyyy don't we get better error line numbers from macro-defined functions?
;  Nope.  It's not the macros.  It's the way we catch errors in runtime.  Need to rethrow that shit somehow...  See "Error ticking entity" handler...


;Finalize the rendering system.  Docs, and tests.
;   So we can move on to input...

;TODO: Start documenting the renderer so we can figure out what its features need to be.  Don't just start implementing stuff willy nilly. 

;TODO: Figure out how to do tests
;How to test renderer?  (Screenshots??)

;TODO: Rendering two games at once.  A child game?  Waahhh..


(require posn)
(define-component Position posn?)
(define-component Counter number?)

#;
(begin

  (define dead-sprite
    (register-sprite
      (h:circle 5 'solid 'red))) 

  (define live-sprite
    (register-sprite
      (h:circle 5 'solid 'green))) 

  (define g 
    (game
      (entity 
        (Position (posn 200 200))
        (Counter 0 (+ 1 (get-Counter)))
        (Sprite live-sprite
                (if (odd? (get-Counter))
                  live-sprite
                  dead-sprite)))))  


  (play! g) )

(begin
  
  (define-component Weapon  entity?)
  (define-component Shooter boolean?)
  (define-component Killer boolean?)

  (define (bullet c) 
    (entity 
      (Position (posn -1 -1) 
                (let ()
                  (posn-add 
                    (posn (random -1 2)
                          (random -1 2))
                    (get-Position)
                    )))
      (Sprite (register-sprite (h:circle 5 'solid c)) 
              (get-Sprite))
      (Counter 0 
               (+ 1 (get-Counter)))
      (Killer  #f 
               (if (= 50 (get-Counter))
                 (despawn)
                 #f))))

  (bullet 'green)
  (bullet 'red)
  (bullet 'blue)

  (define (move-down)
    (posn-add (get-Position)
              (posn 0 5)))

  (define (move-up)
    (posn-add (get-Position)
              (posn 0 -5)))

  (define (move-left)
    (posn-add (get-Position)
              (posn -5 0)))

  (define (move-right)
    (posn-add (get-Position)
              (posn 5 0)))


  (define-component Rotating-Counter number?)
  (define-component Direction number?)

  (define (on-edge)
    (define p (get-Position))
    
    (or 
      (> (posn-x p) 400) 
      (< (posn-x p) 0)
      (> (posn-y p) 400) 
      (< (posn-y p) 0)))

  (define (bounce)
    (define p (get-Direction))
    
    (posn (* -1 (posn-x p))
          (* -1 (posn-y p))))

  (define (e)
    (entity
      (Counter 0 (+ (get-Counter) 1))

      (Rotating-Counter 0 (remainder (get-Counter) 100))

      (Direction (posn 0 0)
                 ;Should vary from 0 to 3, but should only change every 10 ticks.
                 ;  Or if it is on the edge...
                 (cond
                   [(on-edge) (bounce)]
                   [(= 0 (get-Rotating-Counter)) 
                    (list-ref
                      (list
                        (posn -1 0) 
                        (posn 1 0) 
                        (posn 0 -1) 
                        (posn 0 1)) 
                      (random 4))]
                   [else (get-Direction)]))

      (Position (posn (random 200)
                      (random 200)) 
                (posn-add
                  (get-Position)
                  (get-Direction)))

      (Sprite (register-sprite (h:circle 20 'solid (h:make-color (random 255)
                                                                 (random 255) 
                                                                 (random 255)
                                                                 100)))
              (get-Sprite))



      (Weapon (bullet 'green) 
              (if (odd? (get-Counter))
                (bullet 'red)    
                (bullet 'green)))

      (Shooter #f
               (let 
                 ([current-bullet (get-Weapon)])

                 (spawn 
                   (move-to (get-Position) current-bullet))))))

  (define g
           (game (e)
                 (e) 
                 (e)))
  (play! g)

  #;
  (play g)



  )


  

  #;
(begin

  (define dead-sprite
    (register-sprite
      (h:circle 5 'solid 'red))) 

  (define live-sprite
    (register-sprite
      (h:circle 5 'solid 'green))) 

  (define (augment g)
    (define (aug-e e)
      (define c (get-component e conway?))

      ;TODO: make add-components
      (if c

        (add-components e
                        (Position
                          (posn
                            (+ 50 (* 10 (conway-x (get-conway c))))
                            (+ 100 (* 10 (conway-y (get-conway c))))))
                        (Sprite dead-sprite
                                (if (conway-alive? (get-conway))
                                  live-sprite 
                                  dead-sprite)))
        e))

    (apply game (map aug-e (game-entities g))))


  (require "../../../core/test/conway-impl.rkt")

  (define donut
    '((* * *)
      (* _ *)
      (* * *)))

  (define square-3 (square 3))

  (define padded-donut
    (overlay  
      (square 11)
      donut))

  (define quilt
    (beside
      (above donut square-3)
      (above square-3 donut)))

  (define padded-donut3
    (beside (beside padded-donut padded-donut) padded-donut))

  (define quilted-donut
    (above quilt padded-donut3))

  (define quilted-donut2
    (above quilted-donut quilted-donut))

  (define to-play
    (augment 
      (conway-game quilted-donut)))


  #;
  (play to-play)

  #;
  (play! to-play)  

  )




