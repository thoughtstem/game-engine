#lang racket

(require "../game-entities.rkt")


(provide (struct-out circle-collider))

(struct circle-collider [r])

(define (remove-bb-and-add-cc g e c)
  (define radius (circle-collider-r (get-component e circle-collider?)))
  (add-circle-collider e radius))

(new-component circle-collider? remove-bb-and-add-cc)


;this need to run on start 
;(if (get-component ent circle-collider?)  ent) 