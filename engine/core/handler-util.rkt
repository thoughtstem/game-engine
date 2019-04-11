#lang racket

(provide log apply-op compose-handlers for-ticks on-rule remove-self is-handler?)

(require "./crud.rkt"
         "./base.rkt")

(define (is-handler? h) 
  (and (procedure? h)
       (= 3 (procedure-arity h))))

;HANDLERS


(define/contract (remove-self)
  (-> handler?)
  (lambda (g e c)
    (remove-component e c)))

(define/contract (log msg)
  (-> string? handler?)
  (lambda (g e c)
    (displayln msg) 
    'noop))

(define/contract (on-rule r h)
  (-> rule? handler? handler?)

  (lambda (g e c)
    (if (r g e c)
        (h g e c) ;What is wrong with this???
        e ;noop
        )
    ))

(define/contract (compose-handlers . hs)
   (->* () () #:rest (listof handler?) handler?)

   (lambda (g e c) 
     (define temp-e (struct-copy entity e))

     (for ([h hs])
       (define op (h g temp-e (get-component temp-e c)))    

       (set! temp-e
         (cond 
           [(entity? op) op]
           [(component? op) (update-component temp-e c op)]
           [else (raise "What was that?")])))

     temp-e))


(define (apply-op o g e c)
  (cond
    [(component? o) (begin 
                      (update-entity g
                                     e
                                     (update-component e c o)))]
    [(entity? o) (begin 
                   (update-entity g e o))]
    [else (raise (~a "Unsupported handler return value: " o))]))



(define/contract (for-ticks n h)
  (-> number? handler? handler?)

  (define to-go 0)

  (lambda (g e c)
    (set! to-go (add1 to-go)) 

    ;TODO: Should we be looking at h and bailing early if it finishes early?  Or is the point of for-ticks that it spends that long on it no matter what...
    (if (> to-go n)
      (component-done c)
      (h g e c))))


