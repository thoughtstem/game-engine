#lang racket

(provide (struct-out storage)
         get-storage
         get-storage-data
         set-storage

         entity-with-storage)

(require "../game-entities.rkt")


(struct storage (name data))

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
                 (storage name v)))


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