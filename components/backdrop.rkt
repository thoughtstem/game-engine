#lang racket

(require "../game-entities.rkt")
(require "./animated-sprite.rkt")
(require "../entity-helpers/sprite-util.rkt")
(require 2htdp/image)

(require posn)

(provide (struct-out backdrop)
         bg->backdrop
         create-backdrop
         change-tile-to
         next-tile
         change-backdrop
         backdrop-eq?
         more-tiles?
         render-tile
         get-current-tile)

(define handler-function? (-> game? entity? entity?))

(struct backdrop (id tiles columns current-tile))

(define (update-backdrop g e c) e)

;(new-component backdrop?
;               update-backdrop)

; === POWERTOOLS ===
(define/contract (bg->backdrop bg #:rows rows #:columns columns #:start-tile [current 0])
  (-> image? #:rows integer? #:columns integer? #:start-tile integer? backdrop?)
  (backdrop (random 1000000) (sheet->costume-list bg columns rows (* rows columns)) columns current))

; === COMPONENTS ===
;separate create-backdrop component created to keep backdrop id field internal
(define/contract (create-backdrop tiles columns current-tile)
  (-> list? integer? integer? backdrop?)
  (backdrop (random 1000000) tiles columns current-tile))

; === HANDLER FUNCTIONS ===
(define (next-tile direction)
  (-> symbol? handler-function?)
  (lambda (g e)
    (define backdrop         (get-component e backdrop?))
    (define total-tiles      (length (backdrop-tiles backdrop)))
    (define col              (backdrop-columns backdrop))
    (define current-bg-index (get-current-tile e))
    (define next-bg-index    (next-backdrop-index direction total-tiles col current-bg-index))
    (if next-bg-index
        (update-entity ((set-current-tile next-bg-index) g e) ;(update-entity e counter? (counter next-bg-index))
                       animated-sprite? (new-sprite (pick-tile backdrop next-bg-index)))
        e)))

;Renders start-tile from the bg-backdrop component
(define/contract (show-backdrop)
  (-> handler-function?)
  (lambda (g e)
    (define bg-component (get-component e backdrop?))
    ((change-sprite (new-sprite (render-tile bg-component))) g e)))

;Updates bg-backdrop component
(define/contract (change-backdrop backdrop)
  (-> backdrop? handler-function?)
  (lambda (g e)
    ((show-backdrop) g (update-entity e backdrop? backdrop))))

;Change backdrop tile 
(define/contract (change-tile-to num)
  (-> integer? handler-function?)
  (lambda (g e)
    (define bg-entity (get-entity "bg" g))
    (define backdrop (get-component bg-entity backdrop?))
    (update-entity ((set-current-tile num) g e) animated-sprite? (new-sprite (pick-tile backdrop num)))
    ))

(define/contract (set-current-tile num)
  (-> integer? handler-function?)
  (lambda (g e)
    (define bg-entity (get-entity "bg" g))
    (define bg-backdrop (get-component bg-entity backdrop?))
    (update-entity bg-entity backdrop? (struct-copy backdrop bg-backdrop
                                           [current-tile num]))))

; === RULES ===
; Assumes background is named "bg" and has a backdrop component
(define (more-tiles? direction)
  (lambda (g e)
    (define backdrop         (get-component (get-entity "bg" g) backdrop?))
    (define total-tiles      (length (backdrop-tiles backdrop)))
    (define col              (backdrop-columns backdrop))
    (define current-bg-index (backdrop-current-tile backdrop))
    (define next-bg-index    (next-backdrop-index direction total-tiles col current-bg-index))
    (if next-bg-index
        #t
        #f)))

;Compares id fields of backdrop components
(define (backdrop-eq? backdrop)
  (lambda (g e)
  (define bg-backdrop (get-component e backdrop?))
  (eq? (backdrop-id bg-backdrop) (backdrop-id backdrop))))

; === HELPER FUNCTIONS ===
(define/contract (get-current-tile e)
  (-> entity? integer?)
  (backdrop-current-tile (get-component e backdrop?)))

(define/contract (render-tile backdrop)
  (-> backdrop? image?)
  (pick-tile backdrop (backdrop-current-tile backdrop)))

(define/contract (pick-tile backdrop i)
  (-> backdrop? integer? image?)
  (list-ref (backdrop-tiles backdrop) i))

(define (next-backdrop-index direction total-tiles col current-backdrop-index)
  (-> symbol? integer? integer? integer? (and/c integer? boolean?))  
  (define left-edge-list   (range 0 total-tiles col))
  (define right-edge-list  (range (sub1 col) total-tiles col))
  (define top-edge-list    (range 0  col))
  (define bottom-edge-list (range (- total-tiles col) total-tiles))
  (cond [(eq? direction 'left)  (if (member current-backdrop-index left-edge-list)
                                    #f
                                    (sub1 current-backdrop-index))]
        [(eq? direction 'right) (if (member current-backdrop-index right-edge-list)
                                    #f
                                    (add1 current-backdrop-index))]
        [(eq? direction 'top)    (if (member current-backdrop-index top-edge-list)
                                    #f
                                    (- current-backdrop-index col))]
        [(eq? direction 'bottom)    (if (member current-backdrop-index bottom-edge-list)
                                    #f
                                    (+ current-backdrop-index col))]))
