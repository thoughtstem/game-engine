#lang scribble/manual
@require[@for-label[game-engine/engine/core
                    racket/base]]

@(require scribble/lp-include)

@title{game-engine v2}
@author{thoughtstem}

Gotta pick a better name

This doc is (opinionatedly) backwards, starting with a motivating example.  The API docs are at the end.

@lp-include["./rpg-demo.scrbl"]

@include-section["./core/main.scrbl"]
@include-section["./components.scrbl"]
@include-section["./issues.scrbl"]
