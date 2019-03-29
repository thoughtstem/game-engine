#lang racket

(provide check-contract check-game-property check-runs-in-under has-#-entities? has-a-player? player-at? speed-test bg player enemy
         (all-from-out "../main.rkt")
         (all-from-out rackunit))

(require rackunit "../main.rkt")

;====== A little language for doing tests on games ====

;Gives better error messages.
;  Plus lets us test our contracts...
(define-syntax-rule (check-contract c v)
  (let ()
    (define/contract (temp x)
      (-> c boolean?)
      #t)
    (check-pred temp v)))

(define-syntax-rule (check-game-property pred g)
  (check-contract
   (game-has-property/c pred)
   g))

(define-syntax-rule (check-runs-in-under #:miliseconds m
                                         #:message msg
                                         expr)
  (let ()
    (define start-time (current-inexact-milliseconds))

    (define ret expr)
  
    (define end-time (current-inexact-milliseconds))

    (check-pred (curryr < m) (- end-time start-time) msg)

    (displayln (~a "Benchmark ran in: " (- end-time start-time) "\n   " msg))
    (displayln "\n********")
    
    ret))

;===========
;Properties worth checking.  Used across many tests.

(define (has-#-entities? n)
  (λ(g)
    (= n (length (game-entities g)))))

(define (has-a-player? g)
  ((game-has-entity-named/c "player") g))

(define (player-at? p)
  (λ(g)
    (define p2 (get-posn
                (get-entity "player" g)))
    (equal? p
            (posn (round (posn-x p2))
                  (round (posn-y p2))))))


(define (speed-test entities
         #:time expected-time
         #:ticks (ticks 100)
         #:message (msg "Game should run"))

  (define g
    (check-runs-in-under #:miliseconds expected-time
                         #:message (format (~a  msg ".  Should take under ~v miliseconds to run ~v ticks") expected-time ticks)
                         (~> entities 
                             (initialize-game _)
                             (tick _ #:ticks ticks))))

  (check-game-property has-a-player? g)
  (check-game-property (player-at? (posn ticks 50)) g))

(define bg (sprite->entity (square 100 'solid 'red)
                           #:name "bg"
                           #:position (posn 0 0)))

(define player (sprite->entity (circle 1 'solid 'green)
                               #:name "player"
                               #:position (posn 0 50)
                               #:components
                               (speed 1)
                               (direction 0)
                               (every-tick (move))))

(define enemy (sprite->entity (square 1 'solid 'green)
                              #:name "enemy"
                              #:position (posn 0 50)
                              #:components
                              (speed 1)
                              (direction 0)
                              (every-tick (move))))


