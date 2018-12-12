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
  (define target-e
    (cond [(string? (lock-to-name c)) (get-entity (lock-to-name c) g)]
          [(procedure? (lock-to-name c)) ((lock-to-name c) g)]
          [else (error "What is this?")]))
  
  (define target-pos (if target-e
                         (get-component target-e posn?)
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
