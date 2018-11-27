#lang racket

(provide start-game)

(provide (all-from-out "./engine/core.rkt")
         (all-from-out "./engine/rendering.rkt"))

(require "./engine/core.rkt")
(require "./engine/rendering.rkt")

(require threading)

(define (start-game . entities)
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
      game++->game:postprocess))


(define (entities->game:preprocess-entities entities)
  (initialize-game entities))

(define (game->game:start-physics game)
  (physics-start (uniqify-ids game)))


(define (game->__->game++:start-game game)
  (lux-start game))

(define (game++->game:postprocess lux-game)
  (final-state lux-game))
