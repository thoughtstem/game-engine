#lang racket

(provide blue-circle-avatar
         red-square-enemy)

(require "../main.rkt"
         2htdp/image)

(define (blue-circle-avatar)
  (entity  
    (name 'avatar)
    (position (posn 200 200)
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
