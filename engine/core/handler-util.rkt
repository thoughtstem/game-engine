#lang racket

(provide log compose-handlers on-rule remove-self is-handler?)

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
        )))

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


