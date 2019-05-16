#lang scribble/lp2
@(require scribble/manual)

@require[@for-label[game-engine/engine/core
                    racket/base]]

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
