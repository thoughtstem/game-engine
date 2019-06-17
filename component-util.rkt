#lang racket

(provide do-many
         loose-component?
         component-or-system?
         
         not-particle-remove?
         not-toast-remove?
         
         not-particle-sprite?
         not-toast-sprite?)

(require "./game-entities.rkt"
         "./components/observe-change.rkt"
         "./components/storage.rkt")

(define (do-many . funs)
  (lambda (g e)
    (foldl (lambda (next accum)
             (next g accum)) e funs)))

;TODO: Add other simple structs to this list or convert all simple structs
(define loose-component?
  (or/c component? #f))

(define (component-or-system? c-or-list)
  ((or/c loose-component? (listof loose-component?)) (flatten c-or-list)))

; COMPONENT PREDICATES
(define (not-particle-sprite? e)
  (define particle-storages (map storage-data (get-storages-with-prefix "particle-" e)))
  (define particle-sprites (and (not (empty? particle-storages))
                                (flatten (map first particle-storages))))
  (define particle-sprite?
    (if particle-sprites
        (λ (c) (member c particle-sprites component-eq?))
        (λ (c) #f)))
  
  (if particle-sprites
      (not/c particle-sprite?)
      (λ (c) #t)))

(define (not-particle-remove? e)
  (define particle-storages (map storage-data (get-storages-with-prefix "particle-" e)))
  (define particle-remove-components (and (not (empty? particle-storages))
                                          (map second particle-storages)))

  (define particle-remove?
    (if particle-remove-components
       ;(apply or/c (map (curry component-eq?) particle-remove-components))
        (λ (c) (member c particle-remove-components component-eq?))
        (λ (c) #f)))

  (if particle-remove-components
      (not/c particle-remove?)
      (λ (c) #t)))

(define (not-toast-sprite? e)
  (define toast-storages (map storage-data (get-storages-with-prefix "toast-" e)))
  (define toast-sprites (and (not (empty? toast-storages))
                             (flatten (list (map first toast-storages)      ;main sprites
                                            (map second toast-storages))))) ;shadow sprites
  (define toast-sprite?
    (if toast-sprites
        (λ (c) (member c toast-sprites component-eq?))
        (λ (c) #f)))
  
  (if toast-sprites
      (not/c toast-sprite?)
      (λ (c) #t)))
    
(define (not-toast-remove? e)
  (define toast-storages (map storage-data (get-storages-with-prefix "toast-" e)))
  (define toast-remove-components (and (not (empty? toast-storages))
                                       (map third toast-storages)))

  (define toast-remove?
    (if toast-remove-components
        ;(apply or/c (map (curry component-eq?) toast-remove-components))
        (λ (c) (member c toast-remove-components component-eq?))
        (λ (c) #f)))

  (if toast-remove-components
      (not/c toast-remove?)
      (λ (c) #t)))
                        