#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(require threading)

(provide conway-game
         conway-x
         conway-y
         conway-alive?
         entity-conway-alive?
         conway?)

(require (prefix-in impl: "./conway-impl.rkt"))

(define-component conway-manager (data))

(define (conway-manager-entity data)
  (entity
    (conway-manager data #:update (update:conway-manager/data^ impl:conway-tick))))

(define (game->conway-manager g)
  (get-component (get-entity g (curryr has-component conway-manager?))
                 conway-manager?))

(define (game->conway-alive? g x y)
  (define cm 
    (game->conway-manager g))  

  (define d (conway-manager-data cm))

  (eq? '*
       (list-ref (list-ref d y)
                 x)))

(define-component conway (alive? x y))

(define (conway-entity x y)
  (entity (conway #f x y #:update 
                  (lambda (g e c)
                           (update-component e conway? 
                                             (curryr set-conway-alive?
                                                     (game->conway-alive? g x y))))))) 


(define (conway-game->symbols g)
  (conway-manager-data (game->conway-manager g)))

(define (print-symbols ls)
  (map displayln ls))

(define (get-entity-conway-alive? e)
  (get e conway? conway-alive?))

(define (all-alive g)
  (define es 
    (rest (game-entities g))) 
  (andmap get-entity-conway-alive? es))

(define (all-dead g)
  (define es (rest (game-entities g))) 

  (not (ormap get-entity-conway-alive? es)) )

(define (conway-game s)
  (apply game
    (conway-manager-entity s)
    (for*/list ([x (range (impl:width s))]
                [y (range (impl:height s))])
      (conway-entity x y))))


(define square
  '((* * *)
    (* * *)
    (* * *)))

(define padded-square
  (impl:overlay square
                (impl:square 5)))



(test-case "Conway"
           (define g0 (conway-game square))

           (define gs (tick-list g0 4))

           ;It's off by one... is that weird??

           (check-true
             (all-alive (second gs)))

           (check-true
             (all-dead (fourth gs))))

(test-case "Bigger Conway's game of life"
           (define g0 (conway-game padded-square))

           (define gs (tick-list g0 3))

           (check-equal?
             (conway-game->symbols (first gs))
             '((_ _ _ _ _)
               (_ * * * _)
               (_ * * * _)
               (_ * * * _)
               (_ _ _ _ _)))


           (check-equal?
             (conway-game->symbols (second gs))
             '((_ _ * _ _)
               (_ * _ * _)
               (* _ _ _ *)
               (_ * _ * _)
               (_ _ * _ _)))


           (check-equal?
             (conway-game->symbols (third gs))
             '((_ _ * _ _)
               (_ * * * _)
               (* * _ * *)
               (_ * * * _)
               (_ _ * _ _))))


