#lang racket

(require "../game-entities.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require "../entity-helpers/movement-util.rkt")
(require "./backdrop.rkt")
;(require 2htdp/image)

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
         stored?)

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