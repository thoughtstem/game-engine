#lang racket

(require "../game-entities.rkt")
(require posn)

(provide spawner-spawn
         spawner-speed
         spawner-accum
         spawner-next
         (rename-out [make-spawner spawner]))

(struct spawner (spawn speed accum next) #:transparent)

(define (make-spawner spawn speed)
  (spawner spawn speed 0 #f))

(define (spawner-ready? s)
  (>= (spawner-accum s)
      (spawner-speed s)))

(define (spawner-reset s)
  (struct-copy spawner s
               [accum 0]
               [next #f]))


(define (next-spawn s)
  (define s2 (spawner-spawn s))
  (if (procedure? s2)
      (s2)
      s2))

(define (spawner-do-spawn e) 
  (lambda (s)
    (define to-spawn (next-spawn s))

    (define pos (get-component e posn?))
    (define offset (get-component to-spawn posn?))
    (define new-x (+ (posn-x pos) (posn-x offset)))
    (define new-y (+ (posn-y pos) (posn-y offset)))
    
    (define new-entity (update-entity to-spawn posn?
                                      (posn new-x new-y)
                                      #;(get-component e posn?)))
    
    (struct-copy spawner s
                 [next new-entity])))

(define (spawner-inc s)
  (struct-copy spawner s
               [accum (add1 (spawner-accum s))]))

(define (update-spawner g e c)
  (define new-c (spawner-inc c))
  (if (spawner-ready? new-c)
      (update-entity e spawner? ((spawner-do-spawn e) new-c))
      (update-entity e spawner? new-c)))

(define/contract (collect-spawns es)
  (-> (listof entity?) (listof entity?))
  (define spawners (filter identity (map (λ(x) (get-component x spawner?)) es)))
  (filter identity (map spawner-next spawners)))

(define (reset-spawners es)
  (define maybe-spawner-reset (lambda (x) (if (spawner-ready? x)
                                              (spawner-reset x)
                                              x)))
  (map (λ(x) (update-entity x spawner? maybe-spawner-reset)) es))

(define (handle-spawns g)
  (define es     (game-entities g))
  (define new-es (collect-spawns es))
  (define all    (append new-es (reset-spawners es)))
  
  (struct-copy game g
               [entities all]))

(new-component spawner?
               update-spawner)

(new-game-function handle-spawns)

