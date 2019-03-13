#lang racket

(module+ test
  (require rackunit
           2htdp/image
           threading)

  (let ()
    
    (define item (sprite->entity (star 10 'solid 'gold)
                                 #:name       "star"
                                 #:position   (posn 0 0)))

    (define item2 (sprite->entity (star 10 'solid 'red)
                                 #:name       "star"
                                 #:position   (posn 0 0)))
    
    (define b (sprite->entity (circle 10 'solid 'red)
                  #:name       "player"
                  #:position   (posn 0 0)
                  #:components
                  (backpack-system #:components (observe-change backpack-changed? update-backpack))
                  (on-key 'i (add-item item))
                  (on-key 'o (add-item item2))
                  ))

    (define g (initialize-game (list b)))


    (define ticked-g-no-item
      (~> g
          (tick _ #:ticks 10) ; Not necessary, but why not run for a few ticks?
          (handle-key-down _ "b")
          (tick _)
          (tick _)))

    (define ticked-g
      (~> ticked-g-no-item
          (handle-key-down _ "i")
          (tick _)
          (tick _)
          (handle-key-down _ "o")
          (tick _)
          (tick _)
          
          ))

    (check-equal? (not (not (get-entity "backpack" ticked-g)))
                  #t
                  "There should be a backpack entity in the game by now")

    (check-equal? (get-backpack-entities (get-entity "player" ticked-g))
                  (list item item2)
                  "There should be two items in the backpack at this time")


    (check-equal? (> (image-height (draw-entity (get-entity "backpack" ticked-g)))
                     (image-height (draw-entity (get-entity "backpack" ticked-g-no-item))))
                  #t
                  "Backpack should expand verticially when you add things to it")
    ))

(provide drop-last-item
         backpack-system)

;(require 2htdp/image)
(require "../game-entities.rkt")
(require "../components/backpack.rkt")
(require "../components/backdrop.rkt")
(require "./sprite-util.rkt")
(require "./movement-util.rkt")
(require "./ui-util.rkt")
(require "../component-util.rkt")
(require "../components/on-key.rkt")
(require "../components/on-start.rkt")
(require "../components/sound-stream.rkt")
(require "../components/observe-change.rkt")
(require "../components/animated-sprite.rkt")
(require "../components/storage.rkt")
(require posn 2htdp/image)

(define (drop-last-item)
  (lambda (g e)
    ;(displayln "DROPPING LAST ITEM")
    (define target-ent (get-last-item e))
    (define current-tile (game->current-tile g))
    (if (not (empty? (get-items e)))
        (let ([new-entity (update-entity
                           (update-entity
                            target-ent
                            active-on-bg? (active-on-bg current-tile))
                           posn? (posn 0 0))])
          ((spawn new-entity #:relative? #t) g
                                             ((remove-item target-ent) g e)))
        e)))

; ==== SYSTEMS ====
(define (backpack-system #:storable-items [storable-item-list #f]
                         #:store-key      [store-key "z"]
                         #:drop-key       [drop-key "x"]
                         #:backpack-key   [backpack-key "b"]
                         #:pickup-sound   [pickup-sound #f]
                         #:drop-sound     [drop-sound    #f]
                         #:backpack-sound [backpack-sound #f]
                         #:max-items      [max-items    10]
                         #:pickup-rule    [rule (λ (g e) #t)]
                         #:components     [c #f]
                                          . custom-components)
  (define selection-image (square 1 'solid (color 0 255 255 100)))
  (precompile! selection-image)
  (define (weapon-changed? g e)
    (get-storage-data "Selected Weapon" (get-entity "player" g)))
  (define backpack-entity
    (sprite->entity (bordered-box-sprite 50 50) ;(new-sprite bg-image#:animate #f)
                    #:name       "backpack"
                    #:position   (posn 0 0) ;(posn 12 (/ HEIGHT 2))
                    #:components (static)
                                 (hidden)
                                 (layer "ui")
                                 (on-start (do-many update-backpack-sprite
                                                    (go-to-pos-inside 'top-right)
                                                    show))
                                 ;(on-key store-key die)
                                 ;(on-key drop-key die)
                                 (on-key backpack-key die)
                                 (observe-change backpack-changed? update-backpack)
                                 (observe-change weapon-changed? update-backpack)
                                 (cons c custom-components)))

  (define (backpack-is-full? g e)
    (define num-items-in-backpack (length (get-backpack-entities e)))
    (>= num-items-in-backpack max-items))

  (define (open-backpack-if-closed)
    (lambda (g e)
      (if (backpack-not-open? g e)
          ((spawn #:relative? #f backpack-entity) g e)
          e)))
  
  (define (storable-item item-name key)
    (on-key key #:rule (and/r storable-items-nearby?
                              (not/r backpack-is-full?)
                              rule) (if pickup-sound
                                                  (do-many (store-nearby-item item-name)
                                                           (open-backpack-if-closed)
                                                           (play-sound pickup-sound))
                                                  (do-many (store-nearby-item item-name)
                                                           (open-backpack-if-closed)
                                                           ))))
  
  (append (list (backpack)
                (on-key backpack-key #:rule backpack-not-open? (if backpack-sound
                                                                   (do-many (display-items)
                                                                            (spawn #:relative? #f backpack-entity)
                                                                            (play-sound backpack-sound))
                                                                   (do-many (display-items)
                                                                            (spawn #:relative? #f backpack-entity))))
                (on-key drop-key #:rule backpack-not-empty? (if drop-sound
                                                                (do-many (drop-last-item)
                                                                         (open-backpack-if-closed)
                                                                         (play-sound drop-sound))
                                                                (do-many (drop-last-item)
                                                                         (open-backpack-if-closed)))))
          (if storable-item-list
              (map (curryr storable-item store-key) storable-item-list)
              (list (on-key store-key #:rule (and/r storable-items-nearby?
                                                    (not/r backpack-is-full?)
                                                    rule)
                            (if pickup-sound
                                (do-many (store-nearby-item)
                                         (open-backpack-if-closed)
                                         (play-sound pickup-sound))
                                (do-many (store-nearby-item)
                                         (open-backpack-if-closed)
                                         )))))))