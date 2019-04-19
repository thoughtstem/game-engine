#lang racket

(provide change-img-hue     ; 0 to 360
         change-img-sat     ; 0 to 100
         change-img-bright  ; 0 to 100

         set-img-hue        ; 0 to 360
         set-img-sat        ; 0 to 100
         set-img-bright     ; 0 to 100
         
         tint-img
         mask
         mask-pixel
         name->color
         name->color-hsb
         name->hue
         name->sat
         
         rgb->hue
         make-color-hue
         scale-to-fit
         iconify-img

         (struct-out color-hsb)
         make-color-hsb

         hsb->color)

(require 2htdp/image)

(define (name->color string)
  (first (image->color-list (square 1 "solid" string))))

(define (name->color-hsb string)
  (color->color-hsb (name->color string)))

(define (name->hue string)
  (define c (name->color string))
  (rgb->hue (color-red c) (color-green c) (color-blue c)))

(define (name->sat string)
  (define c (name->color string))
  (color-hsb-sat (color->color-hsb c)))

(define (mask-pixel color1 color2)
    (if (eq? (color-alpha color1) 0)
        (make-color 0 0 0 0)
         color2))

(define (mask image1 image2)
  (define image1-list (image->color-list image1))
  (define image2-list (image->color-list image2))
  (color-list->bitmap (map mask-pixel image1-list image2-list) (image-width image1) (image-height image1)))
  

(define (tint-img color img)
  (define c (name->color color))
  (define tint-color (make-color (color-red c) (color-green c) (color-blue c) 128))
  ;(displayln tint-color)
  (define tinted-image (overlay (rectangle (image-width img) (image-height img) "solid" tint-color) img))
  (mask img tinted-image))

(provide (rename-out (make-color-hue color-from-hue)))

#|(define/contract (make-color-hue hue [a 255])
  (->* (number?) (number?) color?)
  (cond
    [(< hue 42.5)  (make-color 255
                               (* hue 6)
                               0
                               a)]
    [(< hue 85)    (make-color (- 255 (exact-round (* (- hue 42.5) 6)))
                               255
                               0
                               a)]
    [(< hue 127.5) (make-color 0
                               255
                               (exact-round (* (- hue 85) 6))
                               a)]
    [(< hue 170)   (make-color 0
                               (- 255 (exact-round (* (- hue 127.5) 6)))
                               255
                               a)]
    [(< hue 212.5) (make-color (exact-round (* (- hue 170) 6))
                               0
                               255
                               a)]
    [else          (make-color 255
                               0
                               (- 255 (exact-round (* (- hue 212.5) 6)))
                               a)]))|#

(define/contract (make-color-hue hue [a 255])
  (->* (number?) (number?) color?)
  (cond
    [(< hue 42.5)  (make-color 255
                               (* hue 6)
                               0
                               a)]
    [(< hue 85)    (make-color (- 255 (exact-round (* (- hue 42.5) 6)))
                               255
                               0
                               a)]
    [(< hue 127.5) (make-color 0
                               255
                               (exact-round (* (- hue 85) 6))
                               a)]
    [(< hue 170)   (make-color 0
                               (- 255 (exact-round (* (- hue 127.5) 6)))
                               255
                               a)]
    [(< hue 212.5) (make-color (exact-round (* (- hue 170) 6))
                               0
                               255
                               a)]
    [else          (make-color 255
                               0
                               (- 255 (exact-round (* (- hue 212.5) 6)))
                               a)]))

(define/contract (make-color-hue-equal-brightness hue [a 255])
  (->* (number?) (number?) color?)
  (cond
    [(< hue 85)  (make-color (- 255 (* hue 3))
                             (* hue 3)
                              0
                              a)]
    [(< hue 170) (make-color  0
                              (- 255 (* (- hue 85) 3))
                              (* (- hue 85) 3)
                              a)]
    [else        (make-color  (* (- hue 170) 3)
                              0
                              (- 255 (* (- hue 170) 3))
                              a)]))

(struct color-hsb
  (hue sat bright alpha))

(define (make-color-hsb hue [sat 100] [bright 100] [alpha 255])
  (color-hsb hue sat bright alpha))

(define (color->color-hsb c)
  (define r (/ (color-red c) 255))
  (define g (/ (color-green c) 255))
  (define b (/ (color-blue c) 255))
  (define a (color-alpha c))
  (define mx (max r g b))
  (define mn (min r g b))
  (define d (- mx mn))
  (define hue (cond
                [(= mx mn) (define h 0)
                           (exact-round (* h 60))]
                [(= mx r)  (define h (+ (/ (- g b) d) (if (< g b) 6 0)))
                           (exact-round (* h 60))]
                [(= mx g)  (define h (+ (/ (- b r) d) 2))
                           (exact-round (* h 60))]
                [(= mx b)  (define h (+ (/ (- r g) d) 4))
                           (exact-round (* h 60))]))
  (define sat (if (= mx 0)
                  0
                  (exact-round (* 100 (/ (- mx mn) mx)))))
  (define bright (exact-round (* (max r g b) 100)))
  (make-color-hsb hue sat bright a))

(define (rgb->hue red green blue)
  (define r (/ red 255))
  (define g (/ green 255))
  (define b (/ blue 255))
  (define mx (max r g b))
  (define mn (min r g b))
  (define d (- mx mn))
  (cond
    [(= mx mn) (define h 0)
                 (exact-round (* h 60))]
    [(= mx r)   (define h (+ (/ (- g b) d) (if (< g b) 6 0)))
                 (exact-round (* h 60))]
    [(= mx g)   (define h (+ (/ (- b r) d) 2))
                 (exact-round (* h 60))]
    [(= mx b)   (define h (+ (/ (- r g) d) 4))
                 (exact-round (* h 60))]))
               
  
(define (rgb->sat r g b)
  (define mx (max r g b))
  (define mn (min r g b))
  (if (= mx 0)
      0
      (exact-round (* 100 (/ (- mx mn) mx)))))

(define (rgb->bright r g b)
  (exact-round (* (/ (max r g b) 255) 100)))

(define (hsb->color c)
  (define h (/ (color-hsb-hue c) 360))
  (define s (/ (color-hsb-sat c) 100))
  (define b (/ (color-hsb-bright c) 100))
  (define a (color-hsb-alpha c))
  (define i (exact-floor (* h 6)))
  (define f (- (* h 6) i))
  (define p (* b (- 1 s)))
  (define q (* b (- 1 (* f s))))
  (define t (* b (- 1 (* (- 1 f) s))))
  (define B (exact-floor (* b 255)))
  (define P (exact-round (* p 255)))
  (define Q (exact-round (* q 255)))
  (define T (exact-round (* t 255)))
  (define case (remainder i 6))
  (cond
    [(= case 0) (make-color B T P a)]
    [(= case 1) (make-color Q B P a)]
    [(= case 2) (make-color P B T a)]
    [(= case 3) (make-color P Q B a)]
    [(= case 4) (make-color T P B a)]
    [(= case 5) (make-color B P Q a)]))

(define (change-hue amount c)
  ;(define hsb-c (color->color-hsb c))
  ;(define new-hue (modulo (+ (color-hsb-hue hsb-c) amount) 360))
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [hue (modulo (+ (color-hsb-hue hsb-c) amount) 360)])))))

(define (change-sat amount c)
  ;(define hsb-c (color->color-hsb c))
  ;(define new-sat (max 0 (min 100 (+ (color-hsb-sat hsb-c) amount))))
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [sat (max 0 (min 100 (+ (color-hsb-sat hsb-c) amount)))])))))

(define (change-bright amount c)
  ;(define hsb-c (color->color-hsb c))
  ;(define new-bright (max 0 (min 100  (+ (color-hsb-bright hsb-c) amount))))
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [bright (max 0 (min 100  (+ (color-hsb-bright hsb-c) amount)))])))))

(define (set-hue amount c)
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [hue (modulo amount 360)])))))

(define (set-sat amount c)
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [sat (max 0 (min 100 amount))])))))

(define (set-bright amount c)
  (if (= (color-alpha c) 0)
      c
      (let ([hsb-c   (color->color-hsb c)])
        (hsb->color (struct-copy color-hsb hsb-c
                                 [bright (max 0 (min 100 amount))])))))

(define (change-img-hue amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry change-hue amount) image-list) (image-width image) (image-height image)))

(define (change-img-sat amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry change-sat amount) image-list) (image-width image) (image-height image)))

(define (change-img-bright amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry change-bright amount) image-list) (image-width image) (image-height image)))

(define (set-img-hue amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry set-hue amount) image-list) (image-width image) (image-height image)))

(define (set-img-sat amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry set-sat amount) image-list) (image-width image) (image-height image)))

(define (set-img-bright amount image)
  (define image-list (image->color-list image))
  (color-list->bitmap (map (curry set-bright amount) image-list) (image-width image) (image-height image)))

(define/contract (scale-to-fit i w)
  (-> image? number? image?)
  (scale (/ w (image-width i)) i))

;useful function! provide out or put elsewhere
;creates silhouettes of an image -- turning every pixel
;that is not 100% transparent to one solid color
(define (iconify-img img [t-color 'black])
  
  (define target-color (if (color? t-color)
                           t-color
                           (name->color t-color)))
  
  (define (maybe-color-pixel original-color)
  (mask-pixel original-color target-color))
  
  (define original-list (image->color-list img))
  (define final-list (map maybe-color-pixel original-list))
  (color-list->bitmap final-list (image-width img) (image-height img)))

;(define (grayscale-img img)
;  (define image-list (image->color-list img))
;  (color-list->bitmap (map (curry
