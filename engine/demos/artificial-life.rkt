#lang racket

(require "../extensions/main.rkt")

;On my low-end chromebook, I'm getting 20 FPS with 5 artificial life-forms, each of which spawns a "cloud" of ~50 other entities (250 total).  It could be sped up if things were organized differently (see bullet-cloud.rkt)

(require 2htdp/image)

(define bullet-sprite (register-sprite (circle 6 'solid 'green)))

(define (bullet live-for) 
  (define die-at (+ global-time live-for))

  (entity 
    (position (posn -1 -1) 
              (posn-add
                (posn (random -1 2)
                      (random -1 2))
                (get-position)))

    (sprite bullet-sprite)
    (death  #f 
            (if (>= global-time die-at)
              (despawn)
              #f))))


(define-component shooter boolean?)

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
    (counter 0 (remainder (+ (get-counter) 1)
                          100))

    (direction (posn 0 0)
               (cond
                 [(on-edge) (bounce)]
                 [(= 0 (get-counter)) 
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

    (shooter #f
             (spawn 
               (move-to (get-position) 
                        (bullet 50))))))

(define global-time 0)
(define-component global-time-update void?)

(define g
  (game 
    ;A "time manager"
    (entity
      (global-time-update (void) (set! global-time (add1 global-time))))
    (e)
    (e) 
    (e)
    (e) 
    (e)))

(play! g)  
