#lang racket

(provide blue-circle-avatar
         red-square-enemy
         door-open-close
         ;Do we need to be providing these?
         door-sprite-closed
         door-sprite-open)

(require "../main.rkt"
         2htdp/image)


(define door-sprite-closed
  (register-sprite
    (square 40 'solid 'brown)))

(define door-sprite-open
  (register-sprite
    (square 40 'solid 'black)))

(define door-open-close
  (sprite door-sprite-closed
          (if (near-avatar? 50)
            door-sprite-open 
            door-sprite-closed)))

(define (blue-circle-avatar (p (posn 200 200)))
  (entity  
    (name 'avatar)
    (position p 
              (posn-add 
                (get-position)
                (as-posn (get-current-input))))
    (sprite (register-sprite (circle '20 'solid 'blue)))))


(define-component time-to-live number?)

(define red-square
  (register-sprite (square 20 'solid 'red)))    

(define (red-square-enemy)
  (entity
    (name 'enemy)

    (time-to-live (random 25 150))

    (counter 0 (^ add1))
    (position (posn (random 0 400)
                    (random 0 400)))
    (rotation 0
              (sin
                (/ (get-counter)
                   10)))

    (size 1
          (/ (- (get-time-to-live)
                (get-counter)) 
             (get-time-to-live)))

    (sprite   red-square)

    (death #f
           ;TODO: Could change to whatever other death behaviour we want
           (if (> (get-counter)
                  (get-time-to-live))
             (despawn)
             #f))))
