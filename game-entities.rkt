#lang racket

(provide start-game)

(provide (all-from-out "./engine/core.rkt"))

(require "./engine/core.rkt")
(require "./engine/rendering.rkt")

(require threading)

(define (start-game . entities)
  (~> entities
      
      ;Step 1: Preprocess the provided entities
      entities->game:preprocess-entities
      ;Step 2: Initialize physics
      game->game:start-physics
      ;Step 3: Begin rendering
      game->__->game++:start-game
      ;Step 4: (Game is over), post-process the state
      game++->game:postprocess))


(define (entities->game:preprocess-entities entities)
  (initialize-game entities))

(define (game->game:start-physics game)
  (physics-start (uniqify-ids game)))


(define (game->__->game++:start-game game)
  (lux-start game))

(define (game++->game:postprocess lux-game)
  (final-state lux-game))
