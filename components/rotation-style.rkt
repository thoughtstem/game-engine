#lang racket

(require "../game-entities.rkt")
(require "./direction.rkt")
(require "./animated-sprite.rkt")
(require posn)
(require 2htdp/image)

(provide (rename-out (make-rotation-style rotation-style)))
(provide (except-out (struct-out rotation-style) rotation-style))

(struct rotation-style (mode facing-right?))

(define (make-rotation-style mode)
  (rotation-style mode #t))


(define (flip-frames frames)
  (map flip-horizontal (vector->list frames)))

(define (update-rotation-style g e c)
  (define mode (rotation-style-mode c))
  (define dir (get-direction e))
  (define fr? (rotation-style-facing-right? c))
  (define sprite (get-component e animated-sprite?))
  (define f (animated-sprite-o-frames sprite))
  ;(define rate (animated-sprite-rate sprite))
  (define rot-func (λ(i) (rotate (- dir) i)))
  (cond
    [(eq? mode 'left-right) (cond
                              [(and fr? (< dir 270)(> dir 90))
                               (update-entity (update-entity e
                                                             animated-sprite?
                                                             (struct-copy animated-sprite sprite [frames (list->vector (flip-frames f))]))
                                              rotation-style?
                                              (rotation-style 'left-right (not fr?)))]
                              [(and (not fr?) (or (> dir 270) (< dir 90)))
                               (update-entity (update-entity e
                                                             animated-sprite?
                                                             (struct-copy animated-sprite sprite [frames f]))
                                              rotation-style?
                                              (rotation-style 'left-right (not fr?)))]
                              [else e])]
    [(eq? mode 'face-direction) (update-entity e animated-sprite? (curry sprite-map-original rot-func))]))

(new-component rotation-style?
               update-rotation-style) 