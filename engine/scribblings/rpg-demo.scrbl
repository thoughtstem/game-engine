#lang scribble/lp2
@(require scribble/manual)

@require[@for-label[game-engine/engine/core
                    racket/base]]

Interesting provable properties:
* If the game ran without errors once, you can disable a component's behaviour without removing it, and the game should run without errors still.  Proof: The original game went through that state once. 
* This allows any part of the system to be frozen, while other parts procede.  Nice for experimentation, debugging, etc.

Tutorial time!
Let's make an RPG game!

TODO: Move the latest hero and world definitions here.  Or move this there...
 Basically figure out what goes where.
 
@chunk[<main>
       (require "../extensions/animation/main.rkt")
       (require "../extensions/animation/test/util.rkt")
       (play!
	(game
         input-manager
	 (blue-circle-avatar)))]

;Umm, what are those requires buddy? 
