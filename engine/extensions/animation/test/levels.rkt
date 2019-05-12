#lang racket

(require "../main.rkt"
         2htdp/image)

;TODO: Make sure the enemies can interact with the player, and vice versa
;        Can they be in the same game??  Where should the avatar be?
;        Make it a collection game. (Collisions how?)
;      Ooooor, skip this for now.  Enemies die on their own currently, which can be changed later without
;      messing up the level-manager logic.

;TODO: Abstract observe change
;TODO: Abstract level manager 


(define avatar
  (entity  
    (name 'avatar)
    (position (posn 200 200)
              (posn-add 
                (get-position)
                (as-posn (get-current-input))))
    (sprite (register-sprite (circle '20 'solid 'blue)))))

(define red-square
  (register-sprite (square 20 'solid 'red)))    

(define-component time-to-live number?)

(define (enemy)
  (entity
    (name 'enemy)

    (time-to-live (random 25 150))

    (counter 0 (^ add1))
    (position (posn (random 0 400)
                    (random 0 400)))
    (rotation 0

              (sin
                (/ (get-counter)
                   10))
              
              )

    (size 1
          (/ (- (get-time-to-live)
                (get-counter)) 
             (get-time-to-live)))

    (sprite   red-square)
    

    (death #f
           ;TODO: Change to on collect
           (if (> (get-counter)
                  (get-time-to-live))
             (despawn)
             #f))))

(define (level n)
  (apply 
    game
    input-manager
    avatar
    (map (thunk* (enemy))
         (range (add1 n)))))


(define level-cycle
  (for/stream ([i (in-naturals)])
     (define level-number (remainder i 5))
     (level level-number)))

(define-component level-stream stream?)

;TODO: Need some kind of observe-change system abstraction
(define-component should-advance? boolean?)
(define-component should-advance-changed? boolean?)
(define-component last-should-advance? boolean?)

(define (should-advance?-became b)
  (and (eq? b (get-should-advance?))
       (get-should-advance-changed?)))


(define (sub-game-won? g)
  (define num-enemies
    (length (filter (has-name 'enemy)
                    (game-entities g))))

  (= 0 num-enemies)  

  ;For testing...
  #;
  (< 250 
     (posn-x 
       (parameterize ([CURRENT-GAME g])
         (get 'avatar 'position)))))


(define level-manager
  (entity
    (name 'level-manager)

    (counter 0 (^ add1))

    (should-advance? #f
                     (sub-game-won? (get-sub-game)))

    (should-advance-changed? #f
                             (not (eq? (get-should-advance?)
                                       (get-last-should-advance?))))

    (last-should-advance? #f (get-should-advance?))

    (level-stream level-cycle
                  (if (should-advance?-became #t)
                    (stream-rest (get-level-stream))
                    (get-level-stream)))

    (sub-game (level 0)
              (if (should-advance?-became #t) 
                (stream-first (get-level-stream))
                (tick (get-sub-game))))

    (also-render (level 0)
                 (get-sub-game))))

(define main
  (game
    level-manager))

(play! main)  



