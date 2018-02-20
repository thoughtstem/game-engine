#lang racket

(provide 
         render-ground
         show-corners
         merge-tiles
         render2)

(require 2htdp/image)
(require 2htdp/universe)

(require threading)

(require "./grids.rkt")
(require "./sheets.rkt")


(define (render-ground g mapping)
  (define row->tiles (curry map mapping))
  (define tile-grid (map row->tiles g))
  (define render-row (curry apply beside))
  (apply above (map render-row tile-grid)))




(define (f i)
  (overlay
   (square (image-width i)
           "outline"
           "black")
   i))

(define (show-corners i)
  (define corners (sub-div i))
  (define tl (first corners))
  (define tr (second corners))
  (define bl (third corners))
  (define br (fourth corners))
  (above
   (beside (f tl) (f tr))
   (beside (f bl) (f br))))


(define (ab-tiles getter i)
  (list-ref
   (append
    (sub-div (getter 0 0))
    (sub-div (getter 1 0))
    (sub-div (getter 0 1))
    (sub-div (getter 1 1))
    (sub-div (getter 0 2))
    (sub-div (getter 1 2))) i))

(define (ms pattern combined-tiles base-tile)
  (define res
    (match pattern
    ;Grass on three sides
    ['((g g)
       (g S)) (combined-tiles 0)]    
    ['((g g)
       (S g)) (combined-tiles 1)]
    ['((S g)
       (g g)) (combined-tiles 3)]
    ['((g S)
       (g g)) (combined-tiles 2)]

    ;Grass on two sides
    ['((g g)
       (s S)) (combined-tiles 12)]
    ['((g s)
       (g S)) (combined-tiles 16)]
    ['((s g)
       (g S)) (combined-tiles 0)]

    ;Grass on two sides (rotation 1)
    ['((s g)
       (S g)) (combined-tiles 21)]
    ['((g g)
       (S s)) (combined-tiles 9)]
    ['((g s)
       (S g)) (combined-tiles 1)]

    ;Grass on two sides (rotation 2)
    ['((S s)
       (g g)) (combined-tiles 22)]
    ['((S g)
       (s g)) (combined-tiles 15)]
    ['((S g)
       (g s)) (combined-tiles 3)]

    ;Grass on two sides (rotation 3)
    ['((g S)
       (g s)) (combined-tiles 10)]
    ['((s S)
       (g g)) (combined-tiles 22)]
    ['((g S)
       (s g)) (combined-tiles 2)]

    ;Grass on one side
    ['((s g)
       (s S)) (combined-tiles 12)]
    ['((g s)
       (s S)) (combined-tiles 4)]
    ['((s s)
       (g S)) (combined-tiles 10)]

    ;Grass on one side (rotation 1)
    ['((s s)
       (S g)) (combined-tiles 15)]
    ['((s g)
       (S s)) (combined-tiles 5)]
    ['((g s)
       (S s)) (combined-tiles 9)]

    ;Grass on one side (rotation 2)
    ['((S s)
       (g s)) (combined-tiles 22)]
    ['((S s)
       (s g)) (combined-tiles 7)]
    ['((S g)
       (s s)) (combined-tiles 15)]

    ;Grass on one side (rotation 3)
    ['((g S)
       (s s)) (combined-tiles 10)]
    ['((s S)
       (g s)) (combined-tiles 6)]
    ['((s S)
       (s g)) (combined-tiles 22)]

    ;Grass on no side
    ['((s s)
       (s S)) (combined-tiles 20)]
    ['((s s)
       (S s)) (combined-tiles 17)]
    ['((S s)
       (s s)) (combined-tiles 11)]
    ['((s S)
       (s s)) (combined-tiles 14)]

    ;Grass on all sides
    [`((,a G)
       (,b ,c)) (third (sub-div base-tile))]
    [`((,a ,b)
       (,c G)) (first (sub-div base-tile))]
    [`((,a ,b)
       (G ,c)) (second (sub-div base-tile))]
    [`((G ,a)
       (,b ,c)) (fourth (sub-div base-tile))]

    [else (square 16 "solid" "green")]
    ))
  #;(overlay
   (debug-quad pattern)
   res)
  res
  )

(define (capitalize sym)
  (string->symbol (string-upcase (symbol->string sym))))

(define/contract (render2-tile g r c combined-tiles base-tile)
  (-> (listof list?) number? number? (-> number? image?) image? image?)
  (above
   (beside
    (ms (grid->quad g r c 'tl capitalize) combined-tiles base-tile)
    (ms (grid->quad g r c 'tr capitalize) combined-tiles base-tile))
   (beside
    (ms (grid->quad g r c 'bl capitalize) combined-tiles base-tile)
    (ms (grid->quad g r c 'br capitalize) combined-tiles base-tile))))

(define (debug-sym s)
  (text (symbol->string s) 12 "red"))

(define (debug-quad l)
  (define l2 (grid-map debug-sym l))
  (above
   (beside (grid-get l2 0 0) (grid-get l2 0 1))
   (beside (grid-get l2 1 0) (grid-get l2 1 1))))
 
(define (debug-quads g r c)
 (list
   (debug-quad (grid->quad g r c 'tl capitalize))
   (debug-quad (grid->quad g r c 'tr capitalize))
   (debug-quad (grid->quad g r c 'bl capitalize))
   (debug-quad (grid->quad g r c 'br capitalize))))
  
(define (render2 g combined-tiles base-tile)
  (for/list ([r (range (length g))])
    (for/list ([c (range (length (first g)))])
      (render2-tile g r c combined-tiles base-tile)
      #;(apply beside
             (append
              (debug-quads g r c)
              (list (render2-tile g r c)))))))



(define (merge-tiles g)
  (apply above
         (map (curry apply beside) g)))









