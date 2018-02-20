#lang racket

(provide (struct-out sprite-machine)
         (struct-out transition)
         current-animation-finished?
         current-sprite
         current-sprite-next-frame
         maybe-transition
         update-machine
         transition-to)

(require lens)
(require lens/data/list)

(require "animated-sprites.rkt")


(define (map-over-hash f h)
  (for/hash ([(k v) (in-hash h)]) (values k (f v))))

(struct/lens sprite-machine
  (current
   sprites
   transitions)
  #:transparent)

(struct/lens transition
             (predicate target)
             #:transparent)

(define (update-machine sm)
  (current-sprite-next-frame (maybe-transition sm)))

(define (current-sprite sm)
  (hash-ref (sprite-machine-sprites sm)
            (sprite-machine-current sm)))

(define (set-current-sprite sm new-sprite)
  (struct-copy sprite-machine sm
               [sprites (hash-set (sprite-machine-sprites sm)
                                  (sprite-machine-current sm)
                                  new-sprite)]))

(define current-sprite-lens (make-lens current-sprite set-current-sprite))

(define (current-sprite-next-frame sm)
  (lens-transform current-sprite-lens sm next-frame))

(define (current-animation-finished? sm)
  (animation-finished? (lens-view current-sprite-lens sm)))

(define (current-transitions sm)
  (hash-ref (sprite-machine-transitions sm)
            (sprite-machine-current sm)))


(define (transition-to sm target)
  (struct-copy sprite-machine sm
               [current target]
               #;[sprites (map-over-hash reset-animation (sprite-machine-sprites sm))]))

(define (maybe-transition sm)
  (define t
    (findf (lambda (x)
           ((transition-predicate x) sm))
         (current-transitions sm)))
  (if t
      (transition-to sm (transition-target t))
      sm))



