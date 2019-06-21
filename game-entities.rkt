#lang racket

(provide start-game
         headless)

(provide (all-from-out "./engine/core.rkt")
         (all-from-out "./engine/rendering.rkt"))

(require "./engine/core.rkt")
(require "./engine/rendering.rkt")
(require "./engine/extensions/sound.rkt")

(require threading
         posn)

(define-syntax-rule (headless expr ...)
  (parameterize ([headless? #t])
    expr
    ...))

(define headless? (make-parameter #f))

(define (start-game . entities)
  (if (headless?)
      (~> (filter identity entities)
          ;Filter identity to Remove any #f's.
          ;Kind of silly, but there's at least one psuedo-entity (asset-precompiler ...) that has a side effect and returns #f.
          ;But maybe one day it'll need to be a full-fledged entity.  So that's why it passes its value into start game.
      
          ;Step 1: Preprocess the provided entities
          entities->game:preprocess-entities
          ;Step 2: Initialize physics
          game->game:start-physics)
      (~> (filter identity entities)
          ;Filter identity to Remove any #f's.
          ;Kind of silly, but there's at least one psuedo-entity (asset-precompiler ...) that has a side effect and returns #f.
          ;But maybe one day it'll need to be a full-fledged entity.  So that's why it passes its value into start game.
      
          ;Step 1: Preprocess the provided entities
          entities->game:preprocess-entities
          ;Step 2: Initialize physics
          game->game:start-physics
          ;Step 3: Begin rendering.  Player plays the game.  Game is returned afterward
          game->__->game++:start-game
          ;Step 4: (Game is over), post-process the state before returning it to the caller of start-game
          game++->game:postprocess)))


(define (entities->game:preprocess-entities entities)
  ;A bit of a hack.  We've been assuming that the last entity is
  ;  the background entity -- sets the width and height of the game.
  ;  But we've also been assuming it's positioned by its top-left corner (posn 0 0).
  ;  Everything else is positioned by its center.  So we'll just hack it to have
  ;  the right position here.
  (define bg (last entities))
  (define adjusted-bg (update-entity bg posn? (posn (/ (w bg) 2)
                                                    (/ (h bg) 2))))
  
  (initialize-game (list-set entities (sub1 (length entities)) adjusted-bg)))

(define (game->game:start-physics game)
  (physics-start (uniqify-ids game)))


(define (game->__->game++:start-game game)
  (lux-start game))

(define (game++->game:postprocess lux-game)
  (displayln "=== CLEANING UP SOUND THREADS ===")
  (stop-sound-streams) ;need to add a try-catch to handle improper shutdowns.
  (kill-all-chipmunks (demo-state lux-game))
  (cleanup-renderer!)
  (displayln "=== CLOSING ERROR OUTPUT PORT ===")
  (close-output-port error-out-port)
  (final-state lux-game))



