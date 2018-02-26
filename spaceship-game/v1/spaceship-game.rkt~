#lang racket

(require "../game-engine.rkt"
         "./assets/spaceship-sprite.rkt"
         "./assets/ore-sprite.rkt"
         "./assets/space-bg-generator.rkt")

(define WIDTH  640)
(define HEIGHT 480)

(define bg-entity
  (sprite->entity (space-bg-sprite WIDTH HEIGHT 100)
                  #:position (posn 0 0)
                  #:name     "bg"))

(define (spaceship-entity p)
  (sprite->entity spaceship-sprite
                  #:position   p
                  #:name       "ship"
                  #:components (key-movement 5)
                               (physical-collider)
                               (on-collide "ore"    increase-speed)
                               (health 3)
                               (on-collide "bullet"  reduce-health)
                               ;(key-animator 'none spaceship-animator)
                               ))

(define (increase-speed g e)
  (define increase (lambda (k)
                     (key-movement (add1 (key-movement-speed k)))))
  (update-entity e key-movement? increase))

(define (restore-health g e)
  (update-entity e health? (health 3)))

(define (reduce-health g e)  ;How to make bg flash?
  (define decrease (lambda (k)
                     (health (sub1 (health-amount k)))))
  (update-entity e health? decrease))

(define bullet2
  (sprite->entity (new-sprite (list (circle 3 "solid" "red")
                                    (circle 3 "solid" "orange")
                                    (circle 3 "solid" "yellow")
                                    (circle 3 "solid" "orange")) 1)
                  #:position (posn 100 100)
                  #:name     "bullet2"
                  #:components (every-tick (move-random #:speed 1))
                               (after-time 0 10 die)  ;Gross params
                               (on-collide "ship" die)))


(define bullet
  (sprite->entity (new-sprite (list (circle 5 "solid" "red")
                                    (circle 5 "solid" "orange")
                                    (circle 5 "solid" "yellow")
                                    (circle 5 "solid" "orange")) 1)
                  #:position   (posn 100 100)
                  #:name       "bullet"
                  #:components (every-tick (move-left #:min   0
                                                      #:speed 5))
                               (spawner bullet2 5 0 #f)
                               (after-time 0 50 die)  ;Gross params
                               (on-collide "ship" die)
                               ))

(define (enemy-entity p)
  (sprite->entity (spaceship-animator 'left)
                  #:position   p
                  #:name       "enemy"
                  #:components  
                                (physical-collider)
                                (every-tick (move-up-and-down #:min   0
                                                              #:max   HEIGHT
                                                              #:speed 10))
                                (spawner bullet 20 0 #f) ;Params here are gross
                                ))

(define (ore-entity p)
  (sprite->entity (ore-sprite (random 10))
                  #:position   p
                  #:name       "ore"
                  #:components ;(key-movement 1)
                               (on-collide "ship" randomly-relocate-me)
                                ))

(define (randomly-relocate-me g e)
  (ore-entity (posn (random WIDTH)
                    (random HEIGHT))))


(define (end-screen msg color)
  (new-sprite (list (overlay (text msg 30 color)
                             (rectangle WIDTH HEIGHT "solid" (make-color 0 0 0 100))))
              1))

(define lose-screen (end-screen "GAME OVER!" "yellow"))
(define win-screen  (end-screen "YOU WIN!" "green"))

(define (game-over-screen)
  (sprite->entity lose-screen
                  #:position   (posn (/ WIDTH 2)
                                     (/ HEIGHT 2))
                  #:name       "ui"
                  #:components (hidden)
                               (every-tick maybe-end)))

(define (maybe-end g e)
  (cond
    [(lost? g e) (show-end-screen g e lose-screen)]
    [(won? g e) (show-end-screen g e win-screen)]
    [else e]))

(define (show-end-screen g e sprite)
  (~> e
      (remove-component _ hidden?)
      (remove-component _ every-tick?)
      (update-entity _ animated-sprite? sprite)))

(define (lost? g e)
  (define names (map get-name (game-entities g)))
  (not (member "ship" names)))

(define (won? g e)
  (define              (name-is s e) (string=? s (get-name e)))
  (define ship         (findf (curry name-is "ship") (game-entities g)))
  (define speed        (key-movement-speed (get-component ship key-movement?)))
  (>= speed 10))

(start-game (game-over-screen)
            (spaceship-entity (posn 100 400))
            (ore-entity       (posn 200 400))
            (enemy-entity     (posn 400 200))
            bg-entity)
 
  