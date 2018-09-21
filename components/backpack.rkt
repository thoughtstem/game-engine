#lang racket

(require "../game-entities.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "../entity-helpers/dialog-util.rkt")
(require "./backdrop.rkt")
(require "./animated-sprite.rkt")
(require "./on-start.rkt")
(require "./sound-stream.rkt")
(require "./on-key.rkt")
(require "../component-util.rkt")

(require 2htdp/image)

(require posn)
(provide (rename-out (make-backpack backpack))
         backpack?
         get-items
         set-items
         add-item
         remove-item
         display-items
         store-item
         store-nearby-item
         drop-last-item
         (struct-out item)
         (struct-out storable)
         stored?
         storable-items-nearby?
         backpack-not-empty?
         backpack-is-empty?
         backpack-system
         in-backpack?)

(struct item (entity amount))

(struct backpack (items))


(define (make-backpack . items)
  (define new-items (map (lambda (e) (item e 1)) items))
  (backpack new-items))

(define (update-backpack g e c) e)

(new-component backpack?
               update-backpack)

(struct storable ())

(define (stored? g e)
  (define items (map item-entity (get-items (get-entity "player" g))))
  (member e items entity-eq?))

(define (update-storable g e c)
  (if (stored? g e)
      (die g e)
      e))

(new-component storable?
               update-storable)

(define (get-items e)
  (backpack-items (get-component e backpack?)))

(define (add-item ent [amount 1])
  (lambda (g e)
    (define old-items (get-items e))
    (define new-items (append old-items (list (item ent amount))))
    (update-entity e backpack? (backpack new-items))))

(define (store-item name)
  (lambda (g e)
    (define item-entity (get-entity name g))
    ((add-item item-entity) g e)))

(define (store-nearby-item [name #f])
  (lambda (g e)
    (define (not-disabled-and-storable? ent)
      (and (not (get-component ent disabled?))
           (get-component ent storable?)))
    (define (name-eq? name e)
      (eq? (get-name e) name))
    (define nearby-ents (if name
                            (filter (curry name-eq? name) (filter not-disabled-and-storable? (get-entities-near e g)))
                            (filter not-disabled-and-storable? (get-entities-near e g))
                            ))
    (displayln (map get-name nearby-ents))
    (if (empty? nearby-ents)
        e
        ((add-item (first nearby-ents)) g e))))

(define (drop-last-item)
  (lambda (g e)
    (define item-list (get-items e))
    (define current-tile (get-current-tile (get-entity "bg" g)))
    (if (not (empty? item-list))
        (let ([new-entity (update-entity (update-entity (item-entity (last item-list))
                                                        active-on-bg? (active-on-bg current-tile))
                                         posn? (posn 0 0))])
          ((spawn new-entity) g (update-entity e backpack? (backpack (remove (last item-list) item-list)))))
        e)))

(define (get-item-name item)
  (get-name (item-entity item)))

(define (item-eq? item1 item2)
  (displayln (~a "Item1: " (get-id (item-entity item1))
                 " Item2: " (get-id (item-entity item2)))) 
  (entity-eq? (item-entity item1)
              (item-entity item2)))

(define (remove-item ent [amount 1])
  (lambda (g e)
    (define old-items (get-items e))
    (define new-items (remove (item ent amount) old-items item-eq?))
    (update-entity e backpack? (backpack new-items))))

(define (set-items . items)
  (lambda (g e)
    (define (enity->item e)
      (item e 1))
    (define new-items (map enity->item items))
    (update-entity e backpack? (backpack new-items))))

(define (display-items)
  (lambda (g e)
    (displayln (~a (map get-id (map item-entity (get-items e)))))
    e))

(define (storable-items-nearby? g e)
  (define (not-disabled-and-storable? ent)
      (and (not (get-component ent disabled?))
           (get-component ent storable?)))
  (define nearby-storable-items (filter not-disabled-and-storable? (get-entities-near e g)))
  (not (empty? nearby-storable-items)))

(define (backpack-not-open? g e)
  (not (get-entity "backpack" g)))

(define (backpack-not-empty? g e)
  (not (empty? (get-items e))))

(define (backpack-is-empty? g e)
  (empty? (get-items e)))

(define (draw-backpack image-list)
  (define (scale-and-pad image) (pad (scale-to-fit image 24) 2 2))
  (define scaled-list (map scale-and-pad image-list))
  (define num-of-items (length image-list))
  (define backpack-items (cond [(> num-of-items 1) (apply above scaled-list)]
                               [(= num-of-items 1) (first scaled-list)]
                               [(= num-of-items 0) (rectangle 32 32 "solid" "transparent")]))
  (overlay  backpack-items
           (rectangle (+ 12 (image-width backpack-items)) (+ 12 (image-height backpack-items)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
           (rectangle (+ 16 (image-width backpack-items)) (+ 16 (image-height backpack-items)) "solid"  (make-color 20 20 20 150))))

(define (update-backpack-sprite g e)
  (define (get-frame entity)
    (pick-frame-original (get-component entity animated-sprite?) 1))
  (define image-list (map get-frame (map item-entity (get-items (get-entity "player" g)))))
  (update-entity e animated-sprite? (new-sprite (draw-backpack image-list))))

(define (in-backpack? name)
  (lambda (g e)
    (define items (get-items (get-entity "player" g)))
    (define entity-list (map item-entity items))
    (define name-list (map get-name entity-list))
    (member name name-list)))

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
  (append (list (make-backpack)
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