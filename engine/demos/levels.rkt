#lang racket

(require game-engine 
         "./util.rkt")

;Enemies aren't very mean.  And they die pretty fast. 
;This is for illustration purposes.

(define (level-cycle n)
  (for/stream ([i (in-naturals)])
     (define level-number (remainder i n))
     
     (game 
       (input-manager)
       (blue-circle-avatar)
       (for/list ([j (in-range level-number)])
         (red-square-enemy)))))

(define (current-level-clear? g)
  (define num-enemies
    (length (filter (has-name 'enemy)
                    (game-entities g))))

  (= 0 num-enemies))

(define main
  (game
    (level-manager (level-cycle 10) 
                   current-level-clear?)))

(play! main)    

