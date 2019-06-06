#lang racket

(provide producer-of
         producer
         crafter-of
         crafting?) 

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
         "./storage.rkt"
         "./counter.rkt"
         "./animated-sprite.rkt"
         "./do-every.rkt"
         "./on-rule.rkt"
         "../entity-helpers/dialog-util.rkt"
         ; =============================================
         "../entity-helpers/render-util.rkt"
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

(define (maybe-store-physical-collider e)
  (if (get-component e physical-collider?)
      (~> e
          (add-components _ (storage "physical-collider" (get-component e physical-collider?)))
          (remove-component _ physical-collider?))
      e))

(define (start-movable-and-locked e on-drop show-info?)
  (~> e
        (update-entity _ posn? (posn 0 0))
        (add-components _
                        (movable #:carry-offset (posn 20 0) #:on-drop on-drop #:show-info? show-info?)
                        (lock-to "player" #:offset (posn 20 0)))
        ;(remove-component _ physical-collider?)
        (maybe-store-physical-collider _)
        ))


(define (display-counter #:prefix [prefix ""])
  (lambda (g e)
    (define count (get-counter e))
    (define count-image (draw-dialog (~a prefix count)))
    ((change-sprite (new-sprite count-image)) g e)))

(define/contract (draw-progress-bar amount #:max [max-val 50])
  (->* (exact-nonnegative-integer?) (#:max exact-nonnegative-integer?) image?)
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
  
(define (make-progress-bar amount #:max [max-val 64])
  (define progress-bar-slice ;(if (= max-val 0)
                             ;    empty-image
                                 (square 1 'solid 'lightblue))
   ; )
  (define bg-image (square 1 'solid 'dimgray))
  (precompile! bg-image progress-bar-slice)
  (define bar-width (* amount (/ 64 (max max-val 1))))
  (define bar-sprite (~> progress-bar-slice
                         (new-sprite _ #:animate #f)
                         (set-x-scale bar-width _)
                         (set-y-scale 10 _)
                         (set-x-offset (/ (- bar-width 64) 2) _)
                         ))
  (define border-sprite (~> bg-image
                        (new-sprite _ #:animate #f)
                        (set-x-scale 66 _)
                        (set-y-scale 12 _)))
  ; Not sure why, but the top of the list is added last.
  (list (storage "progress-bar-sprite" bar-sprite)
        bar-sprite
        border-sprite
        )
  )
  
(define (update-progress-bar #:max [max-val 64])
  (lambda (g e)
    (define current-bar-sprite
      (get-component e (curry component-eq? (get-storage-data "progress-bar-sprite" e))))
    (define count (get-counter e))
    (define bar-width (* count (/ 64 max-val)))
    (define new-bar-sprite (~> current-bar-sprite
                               (set-x-scale bar-width _)
                               (set-x-offset (/ (- bar-width 64) 2) _)
                               ))
    (if (= max-val 0)
        e
        (update-entity e
                       (is-component? current-bar-sprite)
                       new-bar-sprite))))

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

  (define bg-image (square 1 'solid 'white #;(make-color 20 20 20 150)))
  
  (precompile! bg-image)
  
  (define bg-sprite (~> bg-image
                        (new-sprite _ #:animate #f)
                        (set-x-scale 68 _)
                        (set-y-scale 14 _)))
  
  (define progress-counter
    (sprite->entity (if (<= build-time 0)
                        empty-image
                        bg-sprite)
                    #:position   (posn 0 0)
                    #:name       progress-entity-name
                    #:components (static)
                                 (hidden)
                                 (if (<= build-time 0)
                                     #f
                                     (list (make-progress-bar 0 #:max build-time)
                                           (do-every 10 (change-progress-by 1 #:max build-time))
                                           ))
                                 (counter 0)
                                 (on-start show)
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
                         (nearest-to-player? #:filter (has-component? on-key?))
                         (not/r (other-entity-locked-to? "player")))
           ;(do-many (spawn to-clone))
           (if (= build-time 0)
               (spawn to-clone)
               (spawn progress-counter))
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

  (define ent-name (get-name (if (procedure? to-clone)
                                 (to-clone)
                                 to-clone)))
  
  (define short-ent-name (~a ent-name
                             #:min-width 14
                             #:max-width 14
                             #:align 'center))
  
  (define progress-entity-name (~a ent-name "-progress-counter"))
  
  (define (build-ready? g e)
    (define progress-bar (get-entity progress-entity-name g))
    (and progress-bar
         (>= (get-counter progress-bar) build-time)))

  (define bg-image (square 1 'solid 'white #;(make-color 20 20 20 150)))
  
  (precompile! bg-image)
  
  (define bg-sprite (~> bg-image
                        (new-sprite _ #:animate #f)
                        (set-x-scale 68 _)
                        (set-y-scale 14 _)))
  
  (define progress-counter
    (sprite->entity (if (<= build-time 0)
                        empty-image
                        bg-sprite)
                    #:position   (posn 0 0)
                    #:name       progress-entity-name
                    #:components (static)
                                 (hidden)
                                 (layer "ui")
                                 (counter 0)
                                 (if (<= build-time 0)
                                     #f
                                     (list
                                      (new-sprite short-ent-name #:y-offset 5
                                                  #:scale 0.5 #:color 'dimgray)
                                      (make-progress-bar 0 #:max build-time)
                                      (do-every 10 (change-progress-by 1 #:max build-time))
                                      ))
                                 (on-start show)
                                 (on-rule (λ (g e) (or (>= (get-counter e) build-time)
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
  (filter identity (list
                    ;(precompiler (map (λ (t) (draw-progress-bar t #:max build-time)) (range 0 (add1 build-time))))
                    (on-key 'enter
                            #:rule (and/r rule
                                          (λ (g e)(get-entity "crafting list" g))
                                          (λ (g e)
                                            (define sel (if (get-entity "crafting list" g)
                                                            (get-counter (get-entity "crafting list" g))
                                                            #f))
                                            (eq? sel selection))
                                          (λ (g e) (not (get-entity progress-entity-name g)))
                                          near-player?
                                          (nearest-to-player? #:filter (and/c (has-component? on-key?)
                                                                              not-tops?
                                                                              not-ui?))
                                          ; the line below prevents holding two crafted items in hand.
                                          (not/r (other-entity-locked-to? "player" #:filter (and/c (has-component? on-key?)
                                                                                                   not-tops?
                                                                                                   not-ui?)))
                                          )
                            (spawn progress-counter)
                            )
                    (observe-change build-ready? (spawn-if-ready to-clone)))
          ))

(define (crafting? name)
  (lambda (g e)
    (if (get-entity (~a name "-progress-counter") g)
        #t
        #f)))



