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




#;
(
 

(define (for-ticks n h)
  (for-ticks* n h identity))

(define (for-ticks! n h)
  (for-ticks* n h component-done))

(define/contract (for-ticks* n h finished)
  (-> number? handler? handler?)

  (define to-go 0)

  (lambda (g e c)
    (set! to-go (add1 to-go)) 

    (if (> to-go n) 
      (finished c) 
      (h g e c))))




(define (times n h)
  (times* n h identity))

(define (times! n h)
  (times* n h component-done))

(define/contract (times* n h finished)
  (-> number? handler? handler?)

  (define to-go 0)

  (lambda (g e c)

    (cond 
      [(> to-go n) (finished c)]
      [else
        (let [(op (h g e c))]    
          (cond
            [(and (component? op)
                  (component-done? op))
             (begin
               (set! to-go (add1 to-go)) 
               ;Don't return op as is, since that would mark c as done. 
               ;Reattach the same handler
               (set-component-update op (component-update c)))]
            [else op]))])))


(define (sequence . h)
  (sequence* h identity))

(define (sequence! . h)
  (sequence* h component-done))

(define/contract (sequence* hs finished)
  (-> (listof handler?) handler?)

  (define current 0)

  (lambda (g e c)

    (cond 
      [(> current (length hs)) (finished c)]
      [else
        (let* [(h (list-ref hs current))
               (op (h g e c))]    
          (cond
            [(and (component? op)
                  (component-done? op))
             (begin
               (set! current (add1 to-go)) 
               c)]
            [else op]))])))





 )




