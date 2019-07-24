#lang racket

(require "../main.rkt"
         "./util.rkt"
         2htdp/image)


(require (prefix-in bullet: "bullet-cloud.rkt"))
(require (prefix-in conway: "conway.rkt"))
(require (prefix-in phys: "physics-bullet.rkt"))
(require (prefix-in anim: "animation.rkt"))

(define indoor-floor
  (entity
    (position (posn 200 200))
    (sprite
      (register-sprite
        (square 400 'solid 'gray)))))

(define (hack g)
 (define with-door
  (add-entity g
   (door #:to indoors
    (position (posn 200 10))
    door-open-close)) ) 

 (define with-avatar
  ;TODO: Bug.  Seems to be ignoring the position...
  (add-entity with-door (avatar 200 200)))

 (define with-input
  (add-entity with-door (input-manager)))

 with-input)

(define (avatar x y)
  (define named-elf
    (add-component anim:elf-avatar
      (name 'avatar)))

  (update-component
    named-elf 
    position?
    (position x y)))

(define (indoors)
  (game
    (input-manager)
    
    (avatar 200 350)   

    (door #:to (thunk* (hack bullet:g))
      (position (posn 10 200))
      door-open-close)

    (door #:to (thunk* (hack phys:g))
      (position (posn 200 390))
      door-open-close)

    (door #:to (thunk* (hack conway:g))
      (position (posn 390 200))
      door-open-close)

    indoor-floor))

(play! 
  (game
    (door-manager
      (indoors))))









