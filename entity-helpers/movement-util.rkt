#lang racket

(provide randomly-relocate-me)

(require "../game-entities.rkt")
(require posn)

(define (randomly-relocate-me min-x max-x min-y max-y)
  (lambda (g e)
    (update-entity e posn? (posn (random min-x max-x)
                                 (random min-y max-y)))))
  