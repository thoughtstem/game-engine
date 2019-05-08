#lang racket

(require "../animated-sprite.rkt"
         "../renderer.rkt")

(require "../../../core/main.rkt")

(require "../../../core/test/conway.rkt"
         threading
         (prefix-in h: 2htdp/image)
         (prefix-in p: profile))

;TODO: Maybe a few more examples to get the feel for crafting logic with signals.  
;  -> Add input?? 
;ADDED!  Now generalize this into some kind of button state manager thing...

;Also clean up the syntax for fetching of data across entities... (Ideally no (CURRENT-GAME))

;Take a stab at physics before doing too much with renderer?
;  Then we'll have a nice 1st prototype with all necessary features.  Just have to figure out how to swap the new core in underneath the old one...
;    How to simulate add-component and remove-component?


;TODO: Immutability is a lie.  See comment in test/define-component.rkt
;  Make a test that explicitly checks this property


;TODO: Need to figure out how to reference component values on other entities.
;      How do you describe the other entity?  How to make queries fast?

;Keep having ideas about using rosette or constraint based programming to do
; * Procedural geneartion
; * Generating entire games, sequences
; * Flockin behavior
;Or other meta stuff:
; * A component containing a game and a behaviour that ticks it...
; * Run subgames within a game...
; * Procedurally create games at runtime, run them, do something with the result.
; * "Bake" a game, by running it and observing its values.  Faster now as a sub-game...
;For the paper we write about this engine:
; * Game-oriented programming?
; * A game-based programming "paradigmn"?

;Finalize the rendering system.  Docs, and tests.
;   So we can move on to input...

;TODO: Start documenting the renderer so we can figure out what its features need to be.  Don't just start implementing stuff willy nilly. 

;TODO: Figure out how to do tests
;How to test renderer?  (Screenshots??)

;TODO: Rendering two games at once.  A child game?  Waahhh..

  




