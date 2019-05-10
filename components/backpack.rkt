#lang racket




(require "../game-entities.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "../entity-helpers/dialog-util.rkt")
(require "../entity-helpers/ui-util.rkt")
(require "../entity-helpers/render-util.rkt")

(require "./animated-sprite.rkt")
(require "./on-start.rkt")
(require "./sound-stream.rkt")
(require "./on-key.rkt")
(require "./storage.rkt")
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
         draw-backpack-bg
         in-game-by-id?
         in-backpack-by-id?
         )

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
    (update-entity e backpack? (new-backpack new-items))
    ))

(define (store-item name)
  (lambda (g e)
    (define item-entity (get-entity name g))
    ((add-item item-entity) g e)))

(define (store-nearby-item [name #f] #:auto-select? [auto-select? #f])
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
        (~> e
            ((set-storage-named "Selected Weapon" (get-name (first nearby-ents))) g _)
            ((add-item (first nearby-ents)) g _)))
        ))




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
    36)
  
  ;(define bg-sprite (get-component e animated-sprite?))

  (define (clone-sprite s)
    (struct-copy animated-sprite s))

  (define (clone-sprites s-list)
    (map clone-sprite s-list))

  ;sprites-list is a list of list of sprites
  (define sprite-list (map (compose clone-sprites
                                    (curryr get-components animated-sprite?)
                                    item-entity)
                           (get-items (get-entity "player" g))))
  (define name-list (map (compose get-name
                                  item-entity)
                         (get-items (get-entity "player" g))))
  
  (define num-items (length sprite-list))
  (define selected-item-index (if (and (get-storage-data "Selected Weapon" (get-entity "player" g))
                                       (not (eq? (get-storage-data "Selected Weapon" (get-entity "player" g)) "None")))
                                  (index-of name-list (get-storage-data "Selected Weapon" (get-entity "player" g))) ;returns #f if not found
                                  (sub1 num-items)))

  (define new-height (* IMAGE-HEIGHT num-items))

  (define offset-sprite-list
    (flatten (for/list ([ls sprite-list]
                        [i (range (length sprite-list))])
               (map (λ (s)
                      (~> s
                          (set-y-offset (+ (/ IMAGE-HEIGHT 2)
                                           (- (* i IMAGE-HEIGHT)
                                              (/ new-height 2))
                                           (get-y-offset s)) _)
                          (set-scale-xy (/ IMAGE-HEIGHT
                                           (image-height (draw-sprite (first ls)))) _)))
                    ls))))

  (define selection-box-offset (if (= num-items 0)
                                   0
                                   (+ (/ IMAGE-HEIGHT 2)
                                      (- (* (or selected-item-index 0) IMAGE-HEIGHT)
                                         (/ new-height 2)))))

  (define selection-image (square 1 'solid (color 0 255 255 100)))
  ;(precompile! selection-image)

  (define selection-box-sprite (new-sprite selection-image
                                           #:x-scale 44
                                           #:y-scale 40
                                           #:y-offset selection-box-offset))

  (~> e
      (remove-components _ animated-sprite?) ; removes all animated-sprites

      ;Adjust the bg size by picking the right animation frame for the background
      ;Adjust its offset a tiny bit.
      #;(add-component _  (~> bg-sprite
                            (set-x-scale 44         _)
                            (set-y-scale new-height _)))

      ;Add in the actual animated sprites of the entities
      (add-components _ (reverse (bordered-box-sprite 50 (if (= num-items 0)
                                                             50
                                                             (+ new-height 10))))
                      selection-box-sprite
                      offset-sprite-list)))

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

(define (in-game-by-id? item-id)
  (lambda (g e)
    (define item-ids (map (curry get-storage-data "item-id")
                          (filter (curry get-storage "item-id") (game-entities g))))
    (if (member item-id item-ids) #t #f)))

(define (in-backpack-by-id? item-id)
  (lambda (g e)
    (define item-ids (map (compose (curry get-storage-data "item-id")
                                item-entity)
                       (get-items (get-entity "player" g))))
    (if (member item-id item-ids) #t #f)))
