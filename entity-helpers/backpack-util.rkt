#lang racket

(provide drop-last-item
         backpack-system)

;(require 2htdp/image)
(require "../game-entities.rkt")
(require "../components/backpack.rkt")
(require "../components/backdrop.rkt")
(require "./sprite-util.rkt")
(require "./movement-util.rkt")
(require "../component-util.rkt")
(require "../components/on-key.rkt")
(require "../components/on-start.rkt")
(require "../components/sound-stream.rkt")
(require posn)

(define (drop-last-item)
  (lambda (g e)
    (define item-list (get-items e))
    (define current-tile (game->current-tile g))
    (if (not (empty? item-list))
        (let ([new-entity (update-entity
                           (update-entity
                            (item-entity (last item-list))
                            active-on-bg? (active-on-bg current-tile))
                           posn? (posn 0 0))])
          ((spawn new-entity) g (update-entity e backpack? (apply backpack (remove (last item-list) item-list)))))
        e)))

; ==== SYSTEMS ====
(define (backpack-system #:storable-items [storable-item-list #f]
                         #:store-key      [store-key "z"]
                         #:drop-key       [drop-key "x"]
                         #:backpack-key   [backpack-key "b"]
                         #:pickup-sound   [pickup-sound #f]
                         #:drop-sound     [drop-sound    #f]
                         #:backpack-sound [backpack-sound #f])
  (define backpack-entity
    (sprite->entity (draw-backpack '())
                    #:name       "backpack"
                    #:position   (posn 0 0) ;(posn 12 (/ HEIGHT 2))
                    #:components (static)
                                 (hidden)
                                 (on-start (do-many update-backpack-sprite
                                                    (go-to-pos-inside 'top-right)
                                                    show))
                                 (on-key store-key die)
                                 (on-key drop-key die)
                                 (on-key backpack-key die)))
  (define (storable-item item-name key)
    (on-key key #:rule storable-items-nearby? (if pickup-sound
                                                  (do-many (store-nearby-item item-name)
                                                           (open-dialog backpack-entity)
                                                           (play-sound pickup-sound))
                                                  (do-many (store-nearby-item item-name)
                                                           (open-dialog backpack-entity)))))
  (append (list (backpack)
                (on-key backpack-key #:rule backpack-not-open? (if backpack-sound
                                                                   (do-many (display-items)
                                                                            (open-dialog backpack-entity)
                                                                            (play-sound backpack-sound))
                                                                   (do-many (display-items)
                                                                            (open-dialog backpack-entity))))
                (on-key drop-key #:rule backpack-not-empty? (if drop-sound
                                                                (do-many (drop-last-item)
                                                                         (open-dialog backpack-entity)
                                                                         (play-sound drop-sound))
                                                                (do-many (drop-last-item)
                                                                         (open-dialog backpack-entity)))))
          (if storable-item-list
              (map (curryr storable-item store-key) storable-item-list)
              (list (on-key store-key #:rule storable-items-nearby? (if pickup-sound
                                                                        (do-many (store-nearby-item)
                                                                                 (open-dialog backpack-entity)
                                                                                 (play-sound pickup-sound))
                                                                        (do-many (store-nearby-item)
                                                                                 (open-dialog backpack-entity))))))))