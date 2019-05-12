#lang racket 

(require "../main.rkt"
         2htdp/image)

;TODO: Clean this file up and generalize into more specific examples: level swapping when some objective is complete, warping from one game to another via links, one game throws error / other catches it and continues, finally implement hotswap-dev         

;How to implement level swap?  New zones?  "Link" between games?  Warps?
;  Game manager?
;  Back buffer? 


(define bouncing-ball
  (entity
    (position (posn 200 200))
    (sprite (register-sprite (circle 20 'solid 'blue)))))

(define spinning-square
  (entity
    (position (posn 200 200))
    (sprite (register-sprite (square 20 'solid 'red)))))

(define bb
  (game bouncing-ball)) 

(define ss
  (game spinning-square)) 

;HMMM; starting to look abstractable
(define (get-control-counter)
  (define e
    (get-entity (CURRENT-GAME)
                (has-name 'control)))
  (get-counter e))

(define (get-control-toggle)
  (define e
    (get-entity (CURRENT-GAME)
                (has-name 'control)))
  (get-toggle e))


;Damn, this actually worked -- conditionally swapping between two games.
;  Good job.  Bask in the glory for a bit.   

;Then pop up and ask how this helps inform level swaps, warps, hotswapping for dev.
;    You can now switch between an arbitrary number of sub games organized under one master game.  Sounds like an abstraction of "if" to the level of games...  Any data flow that evaluates to booleans.



(define main
  (game
    (entity
      (name 'control)
      ;TODO: What's the fastest way to slow down a counter?  Figure it out and provide it from counter
      ;Also, what are the other abstractions here?  Mapping of counter numbers to other numbers comes up alot.  Reminds me of tweens, animation curves, domain/range mapping in d3. 
      (counter 0 (^ add1))

      (toggle #f (odd? (floor (/ (get-counter)
                                 10)))))

    (entity  
      (name 'bouncing-ball)
      ;TODO: If error thrown in runtime, blame current entity/component in message
      (sub-game bb 
                (if (get-control-toggle)
                  (tick! (get-sub-game))
                  (get-sub-game)))
      (also-render
        bb 
        (if (get-control-toggle)
          (game)
          (get-sub-game))))

    (entity  
      (name 'spinning-square)
      (sub-game ss 
                (if (not (get-control-toggle))
                  (tick! (get-sub-game))
                  (get-sub-game)))
      (also-render
        ss 
        (if (not (get-control-toggle))
          (game)
          (get-sub-game))))))

(play! main)

#;
(debug
  (play! main))



;Ponder:
;  Can we think of an easy way to make startup time faster (instantaneous)?
;  Would be a giant productivity boost...
;  That trick we did with the debugging seems like it would help here...
;    Hot reload...
;    Extend renering with ability to swap-to a different game without needing to restart 
;      (Can generaly be used for warps -- which can generally be used for tile swapping?  Feels like a fruitful direction...)
;      Generalize that into both dynamically-requiring and swap-to
;    Return control back to some process.  Have that process dynamically require
;    in a new file that provides a game (or runs a game?) 



