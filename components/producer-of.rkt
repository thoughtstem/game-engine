#lang racket

(provide producer-of
         producer
         crafter-of) 

(require "../game-entities.rkt"
         "../entity-helpers/carry-util.rkt"
         "../entity-helpers/sprite-util.rkt"
         "./lock-to.rkt"
         "./observe-change.rkt"
         "./on-start.rkt"
         "./backdrop.rkt"
         "./on-key.rkt"
         "../component-util.rkt"

         ; === These are needed for the progress bar ===
         "./counter.rkt"
         "./animated-sprite.rkt"
         "./do-every.rkt"
         "./on-rule.rkt"
         "../entity-helpers/dialog-util.rkt"
         ; =============================================
         posn
         threading
         2htdp/image)


(define (display-entity e)
  (define i (draw-entity e))
  (define p (get-posn e))
  (define a (get-component e active-on-bg?))
  
  (displayln i)
  (displayln (~a "(posn "
                 (exact-round (posn-x p))
                 " "
                 (exact-round (posn-y p))
                 ")"))
  (displayln (~a "(active-on-bg "
                 (first (active-on-bg-bg-list a))
                 ")"))
  
  e)

(define (add-producer-of-self #:on-drop [on-drop display-entity])
  (lambda (g e)
    (add-components e
                    (producer-of e #:on-drop on-drop))))

(define (producer #:on-drop [on-drop display-entity])
   (on-start (add-producer-of-self #:on-drop on-drop)))

(define (start-movable-and-locked e on-drop show-info?)
  (~> e
        (update-entity _ posn? (posn 0 0))
        (add-components _
                        (movable #:carry-offset (posn 20 0) #:on-drop on-drop #:show-info? show-info?)
                        (lock-to "player" #:offset (posn 20 0)))
        (remove-component _ physical-collider?)  ))


(define (display-counter #:prefix [prefix ""])
  (lambda (g e)
    (define count (get-counter e))
    (define count-image (draw-dialog (~a prefix count)))
    ((change-sprite (new-sprite count-image)) g e)))

(define/contract (draw-progress-bar amount #:max [max-val 50])
  (-> exact-nonnegative-integer? #:max exact-nonnegative-integer? image?)
  (define progress-bar  (if (= max-val 0)
                               empty-image
                               (pad (rectangle (* amount (/ 50 max-val)) 10 "solid" "lightblue")4 4)))
  (define max-progress-bar (if (= max-val 0)
                               empty-image
                               (pad (rectangle 50 10 "solid" "transparent") 0 0)))
  (if (= max-val 0)
      empty-image
      (overlay/align "left" "middle"
                     progress-bar
                     (overlay
                      (rectangle (+ 4 (image-width max-progress-bar)) (+ 4 (image-height max-progress-bar)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
                      (rectangle (+ 8 (image-width max-progress-bar)) (+ 8 (image-height max-progress-bar)) "solid"  (make-color 20 20 20 150))))))

(define (update-progress-bar #:max [max-val 50])
  (lambda (g e)
    (define count (get-counter e))
    (define progress-bar (draw-progress-bar count #:max max-val))
    (if (= max-val 0)
        e
        ((change-sprite (new-sprite progress-bar)) g e))))

(define (change-progress-by amount #:min [min-val 0] #:max [max-val 50])
  (lambda (g e)
    (define progress (+ (get-counter e) amount))
    ((do-many (set-counter progress)
              (update-progress-bar #:max max-val)
              #;(display-counter #:prefix "Progress: ")) g e)
    ))

(define (producer-of to-carry #:on-drop [on-drop display-entity] #:build-time [build-time 0] #:show-info? [show-info? #t] #:rule [rule (λ (g e) #t)])
  (define to-clone
    (if (procedure? to-carry)
        (thunk (start-movable-and-locked (to-carry) on-drop show-info?))
        (start-movable-and-locked to-carry on-drop show-info?)))
  
  (define progress-entity-name (~a (get-name (if (procedure? to-clone)
                                                 (to-clone)
                                                 to-clone)) "-progress-counter"))
  
  (define (build-ready? g e)
    (define progress-bar (get-entity progress-entity-name g))
    (and progress-bar
         (>= (get-counter progress-bar) build-time)))
  
  (define progress-counter
    (sprite->entity #;(draw-dialog "Progress: 0") (if (<= build-time 0)
                                                      empty-image
                                                      (draw-progress-bar 0 #:max build-time))
                    #:position   (posn 0 0)
                    #:name       progress-entity-name
                    #:components (static)
                                 (hidden)
                                 (counter 0)
                                 (on-start show)
                                 (do-every 10 (change-progress-by 1 #:max build-time))
                                 (on-rule (λ (g e) (> (get-counter e) build-time)) die)))

  (define (spawn-if-ready to-spawn)
    (lambda (g e1 e2)
      (if (build-ready? g e2)
          ((spawn to-spawn) g e2)
          e2)))
  (list
   (on-key 'z
           #:rule (and/r rule
                         (λ (g e) (not (get-entity progress-entity-name g)))
                         near-player?
                         nearest-to-player? 
                         (not/r (other-entity-locked-to? "player")))
           ;(do-many (spawn to-clone))
           (spawn progress-counter)
           )
   (observe-change build-ready? (spawn-if-ready to-clone))))

(define (ensure-entity procedure-or-entity)
  (if (procedure? procedure-or-entity)
      (procedure-or-entity)
      procedure-or-entity))

(define (crafter-of to-carry
                    #:on-drop    [on-drop display-entity]
                    #:build-time [build-time 0]
                    #:show-info? [show-info? #t]
                    #:rule       [rule (λ (g e) #t)]
                    #:selection  [selection 0])
  (define to-clone
    (if (procedure? to-carry)
        (thunk (start-movable-and-locked (to-carry) on-drop show-info?))
        (start-movable-and-locked to-carry on-drop show-info?)))
  
  (define progress-entity-name (~a (get-name (if (procedure? to-clone)
                                                 (to-clone)
                                                 to-clone)) "-progress-counter"))
  
  (define (build-ready? g e)
    (define progress-bar (get-entity progress-entity-name g))
    (and progress-bar
         (>= (get-counter progress-bar) build-time)))
  
  (define progress-counter
    (sprite->entity #;(draw-dialog "Progress: 0") (if (<= build-time 0)
                                                      empty-image
                                                      (draw-progress-bar 0 #:max build-time))
                    #:position   (posn 0 0)
                    #:name       progress-entity-name
                    #:components (static)
                                 (hidden)
                                 (counter 0)
                                 (on-start show)
                                 (do-every 10 (change-progress-by 1 #:max build-time))
                                 (on-rule (λ (g e) (or (> (get-counter e) build-time)
                                                       (tile-changed? g e))) die)
                                 ))

  (define (spawn-if-ready to-spawn)
    (lambda (g e1 e2)
      (define current-tile (game->current-tile g))
      (displayln (~a "Spawning on tile: " current-tile))
      (define updated-to-spawn
        (update-entity (ensure-entity to-spawn) active-on-bg? (active-on-bg current-tile)))
      (if (build-ready? g e2)
          ((spawn updated-to-spawn) g e2)
          e2)))
  (list
   (on-key 'enter
           #:rule (and/r rule
                         (λ (g e)(get-entity "crafting list" g))
                         (λ (g e)
                           (define sel (if (get-entity "crafting selection" g)
                                           (get-counter (get-entity "crafting selection" g))
                                           #f))
                           (eq? sel selection))
                         (λ (g e) (not (get-entity progress-entity-name g)))
                         near-player?
                         nearest-to-player? 
                         (not/r (other-entity-locked-to? "player")))
           ;(do-many (spawn to-clone))
           (spawn progress-counter)
           )
   (observe-change build-ready? (spawn-if-ready to-clone))))



