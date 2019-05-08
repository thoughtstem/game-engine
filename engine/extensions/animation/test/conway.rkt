#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image)
         (prefix-in p: profile)
         "./fast-posn.rkt")

(define-component Position posn?)
(define-component Counter number?)

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

(play! to-play)  
