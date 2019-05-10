#lang racket

(require "../main.rkt")

(require 2htdp/image)

(require (prefix-in phys: racket-chipmunk))

;TODO: Hook into collisions somehow?

(phys:set-postsolve!
  (λ (c1 c2)
     #;
     (displayln (~a "Postsolve "
                    (phys:chipmunk-meta c1) " "
                    (phys:chipmunk-meta c2)))
     #t))

(phys:set-presolve!
  (λ (c1 c2)
     #;
     (displayln (~a "Presolve "
                    (phys:chipmunk-meta c1) " "
                    (phys:chipmunk-meta c2)))
     #t))

(phys:set-begin!
  (λ (c1 c2)
     #;
     (displayln (~a "Begin "
                    (phys:chipmunk-meta c1) " "
                    (phys:chipmunk-meta c2)))
     #t))

(phys:set-separate! (λ (c1 c2)
                       #;
                       (displayln (~a "Separate "
                                      (phys:chipmunk-meta c1) " "
                                      (phys:chipmunk-meta c2)))
                       #t))

(define b (phys:box 150 50 10 10 #:meta 1))
(phys:set-velocity! b 50 0)

(define b2 (phys:box 250 50 10 10 #:meta 2))
#;
(phys:set-velocity! b2 -50 0)

(define e
  (entity 
    (position (posn 200 200) 
              (posn (phys:x b)
                    (phys:y b)))

    (sprite (register-sprite (square 10 'solid 'green)))))

(define e2
  (entity 
    (position (posn 200 200) 
              (posn (phys:x b2)
                    (phys:y b2)))

    (sprite (register-sprite (square 10 'solid 'red)))))


(define-component physics-world any/c)

(define physics-manager
  (entity
    (physics-world (void)
                   (phys:step-chipmunk (/ 1 120.0)))))


(define g
  (game physics-manager e e2))

(play! g)  
