#lang racket

(module+ test
  ;Some speed benchmarks

  (avatar-crosses-screen-shooting-bullets #:ticks 10
                                          #:time  10)
  
  (avatar-crosses-screen-shooting-bullets #:ticks 100
                                          #:time  1000)

  )


 
(require rackunit
         "../main.rkt")


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
                  (round (posn-y p2)))
            )))

;===========

;Define benchmarks

(define (avatar-crosses-screen-shooting-bullets
         #:time expected-time
         #:ticks (ticks 100)
         #:message (msg (~a "Avatar should cross screen shooting bullets for " ticks " ticks "
                            "in under " expected-time " miliseconds")))

  (define bg (sprite->entity (square 100 'solid 'red)
                             #:name "bg"
                             #:position (posn 0 0)))

  (define bullet (sprite->entity (circle 1 'solid 'black)
                                 #:name "bullet"
                                 #:position (posn 0 0)
                                 #:components
                                 (physical-collider)))

  (define player (sprite->entity (circle 10 'solid 'green)
                                 #:name "player"
                                 #:position (posn 0 50)
                                 #:components
                                 (speed 1)
                                 (direction 0)
                                 (every-tick (move))
                                 (every-tick (spawn bullet))))



  (define g
    (check-runs-in-under #:miliseconds expected-time
                         #:message msg
                         (~> (list player bg)
                             (initialize-game _)
                             (tick _ #:ticks ticks))))


  (check-game-property (has-#-entities? (+ ticks 1)) g) ;100 ticks? 99 bullets + a player + a bg
  (check-game-property has-a-player? g)
  (check-game-property (player-at? (posn ticks 50)) g))



;========


