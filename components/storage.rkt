#lang racket

(provide (struct-out storage)
         get-storage
         get-storage-data)

(require "../game-entities.rkt")


(struct storage (name data))

(define (storage-with-name? n)
  (lambda (s)
    (and (storage? s)
         (string=? n (storage-name s)))))

(define (get-storage name e)
  (get-component e (storage-with-name? name) ))

(define (get-storage-data name e)
  (if (get-storage name e)
      (storage-data
       (get-storage name e))
      #f))