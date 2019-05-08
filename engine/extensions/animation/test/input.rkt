
#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt")
(require "../../../core/main.rkt")
(require "./fast-posn.rkt")

(require (prefix-in h: 2htdp/image))

(define-component Position posn?)
(define-component Input posn?)
(define-component Name symbol?)

(define e
  (entity 
    (Position (posn 200 200) 
              (posn-add 
                (get-Position)
                (get-Input
                  (get-entity 
                    (CURRENT-GAME)
                    (lambda (e)
                      (eq?
                        'input-manager
                        (get-Name e)))))))

    (Sprite (register-sprite (h:circle 5 'solid 'green)))))

(define input-manager
  (entity
    (Name 'input-manager)
    (Input #f 
           (begin
             (displayln buttons)
             (posn
               (cond 
                 [(and
                    (first buttons)
                    (second buttons))
                  0]
                 [(first buttons) -1] 
                 [(second buttons) 1]
                 [else 0])
               0
               ))
           )))

(define g
  (game input-manager e))

(play! g)  
