#lang racket

(require "./common.rkt")

(module+ test
  ;Some speed benchmarks

  (speed-test (avatar-that-spawns-bullets) 
              #:ticks 10  #:time 40
              #:message "Avatar should cross screen shooting bullets")

  (speed-test (avatar-that-spawns-bullets) 
              #:ticks 100 #:time 4000
              #:message "Avatar should cross screen shooting bullets")
  
  (speed-test (unique-entities 10)
              #:ticks 100 #:time 4000
              #:message "A game with 10 unique entities"))


(define (unique-entities n)
  (define es (map (thunk* enemy) (range n))) 

  (list player es bg))


(define (avatar-that-spawns-bullets)
  (define bullet (sprite->entity (circle 1 'solid 'black)
                                 #:name "bullet"
                                 #:position (posn 0 0)
                                 #:components
                                 (physical-collider)))


  (list (add-component player 
                       (every-tick (spawn bullet))) 
        bg))

