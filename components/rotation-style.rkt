#lang racket

(require "../game-entities.rkt")
(require "./direction.rkt")
(require "./animated-sprite.rkt")
(require posn)
(require 2htdp/image)
(require threading)

(provide (rename-out (make-rotation-style rotation-style)))
(provide (except-out (struct-out rotation-style) rotation-style))

(struct rotation-style (mode facing-right? facing-right-animation facing-left-animation))

(define/contract (make-rotation-style mode)
  (-> (or/c 'left-right 'face-direction)
      rotation-style?)
  
  (rotation-style mode #t #f #f))

(define (populated? rs)
  (and (rotation-style-facing-right-animation rs)
       (rotation-style-facing-left-animation rs)))

(define/contract (populate rs original)
  (-> rotation-style? animated-sprite? populated?)

  (struct-copy rotation-style rs
               [facing-right-animation original] ;Assume it starts facing right
               [facing-left-animation  (sprite-map flip-horizontal original)]))

(define (tick-animations rs)
  (define left  (rotation-style-facing-left-animation rs))
  (define right (rotation-style-facing-right-animation rs))
  
  (struct-copy rotation-style rs
               [facing-right-animation (next-frame right)] 
               [facing-left-animation  (next-frame left)]))
  

(define/contract (populate-animations-if-necessary rs e)
  (-> rotation-style? entity? rotation-style?)

  (if (populated? rs) ;Orrrrr if the entity's animated-sprite is not what we've previously cached in facing-right or facing-left...
      (tick-animations rs)
      (begin
        (displayln (~a "Populating " (get-name e)))
        (populate rs (get-component e animated-sprite?)))))

(define (switch-animations-if-necessary c e)
  (define mode (rotation-style-mode c))
  (define dir (get-direction e))
  (define sprite (get-component e animated-sprite?))

  (define e-with-new-animation
    (cond
      [(eq? mode 'left-right) (cond
                                [(and (< dir 270) (> dir 90))
                                 (update-entity e animated-sprite?
                                                (rotation-style-facing-left-animation c))]
                                [(and (or (> dir 270) (< dir 90)))
                                 (update-entity e animated-sprite?
                                                (rotation-style-facing-right-animation c))]
                                [else e])]
    
      #;[(eq? mode 'face-direction) (update-entity e animated-sprite? (curry sprite-map-original rot-func))]))
  
  (update-entity e-with-new-animation rotation-style? c))


(define (update-rotation-style g e c)
  (~> c
      (populate-animations-if-necessary _ e) ;Only has overhead on first component update
      (switch-animations-if-necessary _ e)))

(new-component rotation-style?
               update-rotation-style) 