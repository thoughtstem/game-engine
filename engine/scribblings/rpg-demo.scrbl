#lang scribble/lp2
@(require scribble/manual)

@require[@for-label[game-engine/engine/core
                    racket/base]]

Let's make an RPG game!
 
@chunk[<main>
       (require "../extensions/animation/main.rkt")
       (require "../extensions/animation/test/util.rkt")
       (play!
	(game
         input-manager
	 (blue-circle-avatar)))]

;Umm, what are those requires buddy? 
