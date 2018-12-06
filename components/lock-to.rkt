#lang racket

(require "../game-entities.rkt")
(require posn)

(provide (rename-out (make-lock-to lock-to))
         lock-to-name
         lock-to?
         other-entity-locked-to?)

(struct lock-to (name offset))

(define (make-lock-to name #:offset [offset (posn 0 0)])
  ;(displayln (~a "LOCKING TO: " name))
  (lock-to name offset))

(define (update-lock-to g e c)
  (define target-pos (if (get-entity (lock-to-name c) g)
                         (get-component (get-entity (lock-to-name c) g) posn?)
                         (posn 0 0)))
  (define offset-pos (lock-to-offset c))
  (define new-posn (posn (+ (posn-x target-pos) (posn-x offset-pos))
                         (+ (posn-y target-pos) (posn-y offset-pos))))
  (update-entity e posn? new-posn))

(new-component lock-to?
               update-lock-to)

(define (other-entity-locked-to? s)
  (λ(g e)
    (define other-lock-tos
      (filter identity
              (map (λ(e) (get-component e lock-to?))
                   (game-entities g))))

    (member s (map lock-to-name other-lock-tos))  ))
