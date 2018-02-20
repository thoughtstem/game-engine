#lang racket

(require 2htdp/image)
(require "./grids.rkt")

(provide (struct-out sheet))
(provide make-sheet
         sub-div
         sheet->combiner-tiles)

(struct sheet (image tiles get) #:transparent)

(define (make-sheet img tile-size)
  (define tiles (image->tile-grid img tile-size))
  (define s (sheet img
                   tiles
                   (lambda (x y) (grid-get tiles y x))))
  s)

(define (image->tile-grid img tile-size)
  (for/list ([y (floor (/ (image-height img) tile-size))])
    (for/list ([x (floor (/ (image-width img) tile-size))])
      (tile x y img tile-size))))

(define (tile x y img tile-size)
  (crop (* x tile-size)
        (* y tile-size)
        tile-size
        tile-size
        img))


(define (half-width i)
  (/ (image-width i) 2))



(define (corner x y i)
  (define hw (half-width i))
  (crop (* x hw) (* y hw) 
        hw
        hw
        i))

(define (tl-corner i)
  (corner 0 0 i))

(define (tr-corner i)
  (corner 1 0 i))

(define (bl-corner i)
  (corner 0 1 i))

(define (br-corner i)
  (corner 1 1 i))


(define (sub-div i)
  (list (tl-corner i)
        (tr-corner i)
        (bl-corner i)
        (br-corner i)))

(define (sheet->combiner-tiles s off-x off-y)
  (flatten (list
            (list (sub-div ((sheet-get s) off-x off-y))       (sub-div ((sheet-get s) (+ 1 off-x) off-y)))
            (list (sub-div ((sheet-get s) off-x (+ 1 off-y))) (sub-div ((sheet-get s) (+ 1 off-x) (+ 1 off-y))))
            (list (sub-div ((sheet-get s) off-x (+ 2 off-y))) (sub-div ((sheet-get s) (+ 1 off-x) (+ 2 off-y)))))))


