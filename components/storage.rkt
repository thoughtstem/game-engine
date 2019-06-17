#lang racket

(provide (except-out (struct-out storage) storage)
         (rename-out [new-storage storage])
         
         get-storage
         get-storage-data

         get-storages-with-prefix
         
         set-storage
         set-storage-named
         
         remove-storage
         remove-storage-named
         
         entity-with-storage)

(require "../game-entities.rkt")


(component storage (name data))

(define (storage-with-name? n)
  (lambda (s)
    (and (storage? s)
         (string=? n (storage-name s)))))

(define (storage-with-name-prefix? n)
  (lambda (s)
    (and (storage? s)
         (string-prefix? (storage-name s) n))))

(define (get-storage name e)
  (and e
       (get-component e (storage-with-name? name) )))

(define (get-storages-with-prefix prefix e)
  (and e
       (get-components e (storage-with-name-prefix? prefix))))

(define (set-storage name e v)
  (update-entity e
                 (storage-with-name? name)
                 (new-storage name v)))

(define (remove-storage name e)
  (remove-component e (storage-with-name? name)))

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


; ==== λ (g e) HANDLERS ====
(define (set-storage-named key-name data)
  (lambda (g e)
    (set-storage key-name e data)))

(define (remove-storage-named name)
  (lambda (g e)
    (remove-storage name e)))