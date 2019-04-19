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
    (conway-manager (impl:conway-list->vector data) #:update (update:conway-manager/data^ impl:conway-tick))))

(define (game->conway-manager g)
  (get-component (get-entity g (curryr has-component conway-manager?))
                 conway-manager?))

(define (game->conway-alive? g x y)
  (define cm 
    (game->conway-manager g))  

  (define d (conway-manager-data cm))

  (eq? '*
       (vector-ref (vector-ref d y)
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




