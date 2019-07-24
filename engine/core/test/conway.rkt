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
         conway?
         get-conway
         )

(require (prefix-in impl: "./conway-impl.rkt"))

(define-component conway-data any/c)

(define (conway-manager-entity data)
  (entity
    (conway-data (impl:conway-list->vector data) 
                 (impl:conway-tick (get-conway-data)))))

(define (game->conway-manager g)
  (define cme (first (game-entities g))) 
  (first (entity-components cme)))

(define (game->conway-alive? g x y)

  (begin
    (define cm 
      (game->conway-manager g))  

    (define d (get-conway-data cm))

    (eq? '*
         (vector-ref (vector-ref d y)
                     x)))
  
  )

(define-component conway 
                  list?
                  #;
                  (alive? x y))

(define (conway-alive? c)
  (first c))

(define (conway-x c)
  (second c))

(define (conway-y c)
  (third c))

(define (conway-entity x y)
  (entity (conway (list #f x y) 
                  (let ([new-alive? (game->conway-alive? (CURRENT-GAME) x y)])
                    (list-set (get-conway) 0
                              new-alive?)))))


(define (conway-game->symbols g)
  (get-conway-data (game->conway-manager g)))

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




