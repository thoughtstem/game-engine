#lang racket

(require "../main.rkt")

(require "../../../core/test/conway.rkt"
         2htdp/image)


(define-component counter number?)
(define-component killer boolean?)

(define (bullet color) 
  (define bullet-sprite
    (register-sprite (circle 5 'solid color)))

  (entity 
    (position (posn -1 -1) 
              (posn-add
                (posn (random -1 2)
                      (random -1 2))
                (get-position)))

    (sprite bullet-sprite)
    (counter 0 
             (^ add1))

    (killer  #f 
             (if (= 50 (get-counter))
               (despawn)
               #f))))


(define-component weapon  entity?)
(define-component shooter boolean?)
(define-component rotating-counter number?)
(define-component direction number?)

(define (on-edge)
  (define p (get-position))

  (or 
    (> (posn-x p) 400) 
    (< (posn-x p) 0)
    (> (posn-y p) 400) 
    (< (posn-y p) 0)))

(define (bounce)
  (define p (get-direction))

  (posn (* -1 (posn-x p))
        (* -1 (posn-y p))))

(define (e)
  (entity
    (counter 0 (+ (get-counter) 1))

    (rotating-counter 0 (remainder (get-counter) 100))

    (direction (posn 0 0)
               (cond
                 [(on-edge) (bounce)]
                 [(= 0 (get-rotating-counter)) 
                  (list-ref
                    (list
                      (posn -1 0) 
                      (posn 1 0) 
                      (posn 0 -1) 
                      (posn 0 1)) 
                    (random 4))]
                 [else (get-direction)]))

    (position (posn (random 200)
                    (random 200)) 
              (posn-add
                (get-position)
                (get-direction)))

    (sprite (register-sprite (circle 20 'solid (make-color (random 255)
                                                               (random 255) 
                                                               (random 255)
                                                               100))))



    (weapon (bullet 'green))

    (shooter #f
             (let ([current-bullet (get-weapon)])
               (spawn 
                 (move-to (get-position) current-bullet))))))

(define g
  (game (e)
        (e) 
        (e)
        (e) 
        (e)))

(play! g)  
