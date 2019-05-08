#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image)
         (prefix-in p: profile))

(require "./fast-posn.rkt")
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


(define-component Weapon  entity?)
(define-component Shooter boolean?)
(define-component Killer boolean?)

(define (bullet c) 
  (entity 
    (Position (posn -1 -1) 
              (posn-add*
                (posn (random -1 2)
                      (random -1 2))
                (get-Position)))

    (Sprite (register-sprite (h:circle 5 'solid c)))
    (Counter 0 
             (+ 1 (get-Counter)))

    (Killer  #f 
             (if (= 50 (get-Counter))
               (despawn)
               #f))))

(bullet 'green)
(bullet 'red)
(bullet 'blue)

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
              (posn-add*
                (get-Position)
                (get-Direction)))

    (Sprite (register-sprite (h:circle 20 'solid (h:make-color (random 255)
                                                               (random 255) 
                                                               (random 255)
                                                               100))))



    (Weapon (bullet 'green) 

            #;
            (if (odd? (get-Counter))
              (bullet 'red)    
              (bullet 'green)))

    (Shooter #f
             (let ([current-bullet (get-Weapon)])

               (spawn 
                 (move-to (get-Position) current-bullet))))))

(define g
  (game (e)
        (e) 
        (e)
        (e) 
        (e) 
        ))

(require (prefix-in p: profile/render-graphviz))
(require (prefix-in p: (only-in profile/analyzer analyze-samples)))
(require (prefix-in p: (only-in profile/sampler create-sampler)))

(play! g)  

#;
(define t
  (thread
    (thunk
      (play! g))))

#;
(define sampler
  (p:create-sampler
    t 
    0))

(sleep 10)


#;
(p:render
  (p:analyze-samples
    (sampler
     'get-snapshots)))


