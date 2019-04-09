#lang racket

(provide update-f c-diff?)

(struct add-e    (e-desc))
(struct update-e (e-id c-crud))
(struct remove-e (e-id))

(struct add-c    (e-id c-desc))
(struct update-c (c-id field-crud))
(struct remove-c (c-id))


(struct update-f (c-id f-id func))

(define c-crud? (or/c add-c? update-c? remove-c?))
(define e-crud? (or/c add-e? update-e? remove-e?))
(define f-crud? update-f?)

;A list of changes to a a game
(define g-diff?  (listof (or/c add-e? remove-e?)))

;A list of changes to an entity
(define e-diff? (listof (or/c add-c? remove-c?)))

;A list of changes to an component
(define c-diff? (listof f-crud?))


#;
(require "./crud.rkt")


#;
(define (interp-crud-op! g op)
  (cond
    [(e-crud? op) (interp-e-crud-op! g op)]
    ))

#;
(define (interp-e-crud-op! g op)
  (cond
    [(add-e? op) (add-entity! g (add-e-e-desc op))]
    [(update-e? op) (update-entity! g (update-e-e-id op) (update-e-c-crud op))]
    [(remove-e? op) (remove-entity! )]
    ))


