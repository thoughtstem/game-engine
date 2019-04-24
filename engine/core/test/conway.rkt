#lang racket

(require rackunit 
         "../main.rkt"
         "./util.rkt")

(require threading)

(provide conway-game
         conway-x
         conway-y
         conway-alive?
         game->conway-alive?
         conway?)

(require (prefix-in impl: "./conway-impl.rkt"))

(define-component conway-manager (data))

(define (conway-manager-entity data)
  (entity
    (conway-manager (impl:conway-list->vector data) 
                    #:update 
                    (update:conway-manager/data^ impl:conway-tick))))

(define (game->conway-manager g)
  (define cme (first (game-entities g))) 
  (first (entity-components cme)))

(define (game->conway-alive? g x y)

  (begin
    (define cm 
      (game->conway-manager g))  

    (define d (conway-manager-data cm))

    (eq? '*
         (vector-ref (vector-ref d y)
                     x)))
  
  )

(define-component conway (alive? x y))

(define (conway-entity x y)
  (entity (conway #f x y #:update 
                  (handler (g e c)
                    (define new-alive? (game->conway-alive? g x y))  

                    (update-component e 0
                                      (curryr set-conway-alive? new-alive?))

                    )))) 


(define (conway-game->symbols g)
  (conway-manager-data (game->conway-manager g)))

(define (print-symbols ls)
  (map displayln ls))

(define (get-entity-conway-alive? e)
  #;
  (get e conway? conway-alive?)
  
  ;TODO: Could have grabbed the 3 and cached it if we didn't already know it.
  ;Could be useful to give such a caching function to our users.

  (list-ref (entity-components e) 0) )

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




