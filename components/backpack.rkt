#lang racket




(require "../game-entities.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "../entity-helpers/dialog-util.rkt")

(require "./animated-sprite.rkt")
(require "./on-start.rkt")
(require "./sound-stream.rkt")
(require "./on-key.rkt")
(require "../component-util.rkt")

(require 2htdp/image
         threading)

(require posn)
(provide (except-out (struct-out backpack) backpack)
         (rename-out (make-backpack backpack))
         ;entity->item ; provided for dev only
         ;item->entity ; provided for dev only
         get-items
         set-items
         add-item
         remove-item
         remove-item-by-name
         get-last-item
         display-items
         store-item
         store-nearby-item
         (struct-out item)
         (struct-out storable)
         stored?
         storable-items-nearby?
         backpack-not-open?
         backpack-not-empty?
         backpack-is-empty?
         in-backpack?
         set-backpack-entities
         get-backpack-entities
         draw-backpack
         update-backpack-sprite
         backpack-changed?
         update-backpack
         draw-backpack-bg)

(struct item (entity amount))

(component backpack (items))

(define/contract (entity->item e)
  (-> entity? item?)
  (item e 1))

(define/contract (item->entity i)
  (-> item? entity?)
  (item-entity i))

(define/contract (make-backpack . entities)
  (->* () () #:rest (or/c (listof entity?) empty?) backpack?)
  (define new-items (map (lambda (e) (item e 1)) entities))
  (new-backpack new-items))

;(define (update-backpack g e c) e)

;(new-component backpack?
;               update-backpack)

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

(define (set-backpack-entities entity-with-backpack entities-for-backpack)
  (update-entity entity-with-backpack
                 backpack?
                 (new-backpack (map entity->item entities-for-backpack))))

(define (get-backpack-entities e)
  (map item-entity (get-items e)))

(define (get-items e)
  (backpack-items (get-component e backpack?)))

(define (get-last-item e)
  (item-entity (last (get-items e))))

(define/contract (add-item ent [amount 1])
  (->* (entity?) (number?) procedure?)
  (lambda (g e)
    (define old-items (get-items e))
    (define new-items (append old-items (list (item ent amount))))
    (update-entity e backpack? (new-backpack new-items))))

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




(define (get-item-name item)
  (get-name (item-entity item)))

(define (item-eq? item1 item2)
  (displayln (~a "Item1: " (get-id (item-entity item1))
                 " Item2: " (get-id (item-entity item2)))) 
  (entity-eq? (item-entity item1)
              (item-entity item2)))

(define/contract (remove-item ent [amount 1])
  (->* (entity?) (number?) procedure?)
  (lambda (g e)
    (define old-items (get-items e))
    (define new-items (remove (item ent amount) old-items item-eq?))
    (update-entity e backpack? (new-backpack new-items))))

(define (set-items . items)
  (lambda (g e)
    (define (enity->item e)
      (item e 1))
    (define new-items (map enity->item items))
    (update-entity e backpack? (new-backpack new-items))))

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
  (define (scale-and-pad image) (pad (scale-to-fit image 40) 2 2))
  (define scaled-list (map scale-and-pad image-list))
  (define num-of-items (length image-list))
  (define backpack-items (cond [(> num-of-items 1) (apply above scaled-list)]
                               [(= num-of-items 1) (first scaled-list)]
                               [(= num-of-items 0) (rectangle 32 32 "solid" "transparent")]))
  (overlay  backpack-items
            (rectangle (+ 0 (image-width backpack-items))
                       (+ 0 (image-height backpack-items))
                       'solid 'black)
            
            #;(rectangle (+ 12 (image-width backpack-items)) (+ 12 (image-height backpack-items)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
            #;(rectangle (+ 16 (image-width backpack-items)) (+ 16 (image-height backpack-items)) "solid"  (make-color 20 20 20 150))))


(define (draw-backpack-bg n)
  (define ITEM-SIZE 40)
  
  (overlay
   (rectangle (+ 12 ITEM-SIZE) (+ 12 (* n ITEM-SIZE)) "outline" (pen "white" 2 "solid" "butt" "bevel"))
   (rectangle (+ 16 ITEM-SIZE) (+ 16 (* n ITEM-SIZE)) "solid"  (make-color 20 20 20 150))))


(define (update-backpack-sprite g e)
  (define IMAGE-HEIGHT
    40)
  
  (define bg-sprite (get-component e animated-sprite?))

  (define sprite-list (map (curryr get-component animated-sprite?)
                           (map item-entity
                                (get-items (get-entity "player" g)))))
  
  (define num-items (length sprite-list))

  (define new-height (* IMAGE-HEIGHT num-items))

  (define offset-sprite-list
    (for/list ([s sprite-list]
               [i (range (length sprite-list))])
      (~> s
          (set-y-offset (+ (/ IMAGE-HEIGHT 2)
                           (- (* i IMAGE-HEIGHT)
                              (/ new-height 2))) _)
          (set-scale-xy (/ IMAGE-HEIGHT
                           (image-height (render s))) _))))

  (~> e
      (remove-components _ animated-sprite?)

      ;Adjust the bg size by picking the right animation frame for the background
      ;Adjust its offset a tiny bit.
      (add-component _  (~> bg-sprite
                            (set-x-scale 44         _)
                            (set-y-scale new-height _)))

      ;Add in the actual animated sprites of the entities
      (add-components _ offset-sprite-list)))

(define (in-backpack? name)
  (lambda (g e)
    (define items (get-items (get-entity "player" g)))
    (define entity-list (map item-entity items))
    (define name-list (map get-name entity-list))
    (not (not (member name name-list)))))

(define/contract (remove-item-by-name name [amount 1])
  (->* (string?) (number?) procedure?)
  (lambda (g e)
    (define old-items (get-items e))
    (define target-ent (sprite->entity empty-image
                                       #:name     name
                                       #:position (posn 0 0)))
    (define new-items (remove (entity->item target-ent) old-items item-name-eq?))
    ;(define new-entity-list (map item->entity new-items)) ; warning: loses amount data
    ;(update-entity e backpack? (apply backpack new-entity-list))
    (update-entity e backpack? (new-backpack new-items))
    ))

(define (item-name-eq? item1 item2)
  (displayln (~a "Item1: " (get-name (item->entity item1))
                " Item2: " (get-name (item->entity item2)))) 
  (entity-name-eq? (item->entity item1)
                   (item->entity item2)))

(define (backpack-changed? g e)
  (length (get-items (get-entity "player" g))))

(define (update-backpack g e1 e2)
  (if (void? e1)
      e2
      (begin
        (~> e2
            (update-backpack-sprite g _)
            ((go-to-pos-inside 'top-right) g _)))))


