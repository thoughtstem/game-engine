#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(require threading)

#;
(provide conway-game
         conway-game-set
         conway-x
         conway-y
         conway-alive?
         entity-conway-alive?
         conway?
         overlay
         above
         beside)

(require (prefix-in impl: "./conway-impl.rkt"))

(define-component conway-manager (data))

(define (conway-manager-entity data)
  (entity 
    (conway data #:update (update:conway-manager/data^ impl:conway-tick))))

(define (game->conway-manager g)
  (get-component (get-entity g (has-component conway-manager?))
                 conway-manager?))

(define (game->conway-alive? g x y)
  (define cm 
    (game->conway-manager g))  

  (define d (conway-manager-data cm))

  (eq? '*
       (list-ref (list-ref d y)
                 x)))

(define-component conway (alive?))

(define (conway-entity x y)
  (entity (conway #f #:update 
                  (lambda (g e c)
                           (update-component e conway? 
                                             (curryr set-conway-alive?
                                                     (game->conway-alive? g x y))))))) 


(define (conway-game->symbols g)
  (conway-manager-data (game->conway-manager g)))

(define (print-symbols ls)
  (map displayln ls))

(define (get-entity-conway-alive? e)
  (conway-alive?
    (get-component e conway?)))

(define (all-alive g)
  (define es (game-entities g)) 
  (andmap get-entity-conway-alive? es))

(define (all-dead g)
  (define es (game-entities g)) 

  (not (ormap get-entity-conway-alive? es)) )

(define (conway-game s)
  (game
    (conway-manager-entity s)))


(define donut
  '((* * *)
    (* _ *)
    (* * *)))

(define padded-donut
  (impl:overlay donut
                (impl:square 5)))



(let () ;test-case "Conway's game of life"
           (define g0 (conway-game donut))

           (define gs (tick-list g0 3))

           (check-true
             (all-alive (first gs)))

           (check-true
             (all-dead (third gs))))

(test-case "Bigger Conway's game of life"
           (define g0 (conway-game padded-donut))

           (define gs (tick-list g0 3))

           #;
           (begin
             (print-symbols (conway-game->symbols (first gs)))
             (print-symbols (conway-game->symbols (second gs)))
             (print-symbols (conway-game->symbols (third gs))))

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


