#lang racket

(provide log apply-handler apply-op compose-handlers for-ticks on-rule remove-self 
         
         is-handler?
         apply-op-entity
         apply-op-game)

(require "./crud.rkt"
         "./base.rkt")

(define (is-handler? h) 
  (and (procedure? h)
       (= 3 (procedure-arity h))))

;HANDLERS


(define/contract (remove-self)
  (-> handler?)
  (lambda (g e c)
    (remove-c c)))

(define/contract (log msg)
  (-> string? handler?)
  (lambda (g e c)
    (displayln msg) 
    'noop))

(define/contract (apply-handler h g e c)
  (-> handler? game? entity? component? operation?)

  (define op (h g e c))
  (apply-op op g e c))


(define/contract (on-rule r h)
  (-> rule? handler? handler?)

  (lambda (g e c)
    (if (r g e c)
        (h g e c)
        'noop)))


(define/contract (compose-handlers . hs)
   (->* () () #:rest (listof handler?) handler?)

   (lambda (g e c) 
     (map (lambda (h) (h g e c)) hs)))


(define/contract (apply-op-entity e op)
   (-> entity? c-crud? entity?)
  
   (match op
     [(add-c    c) (add-component e c)]
     [(remove-c c) (remove-component e c)]
     ;[(update-c c new-c) (update-component e c new-c)]
     ))

(define/contract (apply-op-game g op)
   (-> game? e-crud? game?)
  
   (match op
     [(add-e    e) (add-entity g e)]
     [(remove-e e) (remove-entity g e)]
     ;[(update-e e new-e) (update-entity e new-e)]
     ))


(define (apply-op o g e c)
  (cond
    [(component? o) (begin 
                      (displayln "COMPONENT UPDATE")
                      (displayln c) 
                      (displayln o) 
                      (update-entity g
                                     e
                                     (update-component e c o)))]
    [(c-crud? o) (update-entity g
                                e
                                (apply-op-entity e c o))]
    [(e-crud? o) (apply-op-game g o)]
    [(noop? o) g]
    [(done? o) (update-entity g e 
                                 (update-component e c (component-done c)))]
   ;As it stands, a list of ops has the property that some upstream changes will be completely masked by downstream ones -- even if they could theoretically have both happened...  Feels like we need a merge operation.  
    [(list? o) (foldl (lambda (o)
                        (apply-op o g e c)) g o)]
    [else (raise (~a "Unsupported handler return value: " o))]))



(define/contract (for-ticks n h)
  (-> number? handler? handler?)

  (define to-go 0)

  (lambda (g e c)
    (set! to-go (add1 to-go)) 

    ;TODO: Should we be looking at h and bailing early if it finishes early?  Or is the point of for-ticks that it spends that long on it no matter what...
    (if (> to-go n)
      'done
      (h g e c))))


