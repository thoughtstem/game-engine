#lang racket

(provide (rename-out [new-storage storage])
         storage?
         storage-name
         storage-data
         
         get-storage
         get-storage-data
         set-storage
         set-storage-named

         entity-with-storage)

(require "../game-entities.rkt")


(component storage (name data))

(define (storage-with-name? n)
  (lambda (s)
    (and (storage? s)
         (string=? n (storage-name s)))))

(define (get-storage name e)
  (and e
       (get-component e (storage-with-name? name) )))

(define (set-storage name e v)
  (update-entity e
                 (storage-with-name? name)
                 (new-storage name v)))


(define (get-storage-data name e)
  (if (get-storage name e)
      (storage-data
       (get-storage name e))
      #f))

(define (entity-with-storage key val g)
  (findf
   (λ(e)
     (and
      (get-storage key e)
      (equal? val (get-storage-data key e))))
   (game-entities g)))

(define (set-storage-named key-name data)
  (lambda (g e)
    (set-storage key-name e data)))