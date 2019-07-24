#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@title{issues}
@author{thoughtstem}

@section{Maintenance Tasks}

;TODO: Immutability is a lie.  See comment in test/define-component.rkt
;  Make a test that explicitly checks this property
;  By the way, is mutability helping us much?  Maybe we should take it out if it's not...

;TODO: Making render tests automated
;(Screenshots??)


@section{Feature Ideas}

Moonshot:
  Could we render game previews (various projections of the game?) embedded in a DrRacket snip? 
  That sounds like a game changer... 

;Can we use components that store (and tick) other components as a way to implement "systems"?
;Rendering two games at once.  A child game?  
;Components within components.  Entities within components

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

@section{Performance}


;ADD a section about performance. Raw notes below...
;TODO: Make conway fast again.  Query optimizations.  Caching.
;  I tried adding a lookup hash to entities, but I'm not seeing any speed improvement on the red/green poopers...
;    Should remove if it's not helping
;TODO: Roll a profiler (or build on top of Racket's now that we've simplified things again).
;  Done.
;  Testing it.  It seemed to blame Position for being slow, so I changed to an implementation of posn that it claims is faster.
;    But... it doesn't feel faster and the FPS looks the same.
;    (Then again, I need to disable the sampling to have a true test...)

;Have it print the stats over time


;One big problem is that I don't know what I should be expecting from this engine.  Is 20 FPS with 250 entities good or bad?
;  What would happen with unity?
;  What would happen with 2htdp/universe?

;Or another angle:
;  How many math operations per second should Racket be able to do?  How much overhead from garbage?  How much from memory allocations?
;  How many is this engine doing?
;  High level profiler might be able to give us that picture, btw.  So we can keep our estimations honest.

