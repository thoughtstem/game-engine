#lang racket

(provide apply-handler apply-op compose-handlers for-ticks on-rule)

(require "./crud.rkt"
         "./base.rkt")

;TODO: move to base
(define rule? (-> game? entity? component? boolean?))

;HANDLERS

(define/contract (on-rule r h)
  (-> rule? handler? handler?)

  (lambda (g e c)
    (if (r g e c)
        (h g e c)
        #f)))


(define/contract (apply-handler h g e c)
  (-> handler? game? entity? component? operation?)

  (define op (h g e c))
  (apply-op op g e c))

(define/contract (compose-handlers . hs)
   (->* () () #:rest (listof handler-convertable?) handler?)

   (lambda (g e c) 
     (foldl (lambda (f g)
              (define new-e (get-entity g e)) 
              (define new-c (get-component e c)) 

              (apply-handler f 
                              g 
                              ;Get updated versions of e and c...
                              new-e 
                              new-c))
            g
            (map lift-to-handler hs))))

(define (apply-op o g e c)
  (cond
    [(game? o) o]
    [(entity? o) (update-entity g e o)]
    [(component? o) (update-entity g e 
                                   (update-component e c o))]
    [(noop? o) g]
    [(done? o) (update-entity g e 
                                (update-component e c (component-done c)))]
    [(list? o) (foldl (lambda (next-op g)
                        (apply-op next-op g)) 
                      g o)]
    [else (raise (~a "Unsupported handler return value: " o))]))



(define (for-ticks n h)
  (define to-go 0)

  (lambda (g e c)
    (set! to-go (add1 to-go)) 

    ;TODO: Should we be looking at h and bailing early if it finishes early?  Or is the point of for-ticks that it spends that long on it no matter what...
    (if (> to-go n)
      'done
      ((lift-to-handler h) g e c))))


#;
(

(define (do-many . ghs)
  (lambda (g e c)
    (define temp-g (copy-game g)) 
    (for ([gh ghs])
      (set! temp-g ((any-handler->game-handler gh) temp-g e c)))

    temp-g))

(define (reverse-params f)
  (define (new-f . ps)
    (apply f (reverse ps)))
  
  new-f)


(define (once-each . ghs)
  (thunk
    (define done #f)
    (lambda (g e c)
      (if done 'done
          (begin
            (set! done #t) 
            ((apply do-many ghs) g e c)))
      )))


;Wow.  I like how short and generalizable these combinators are.  But I don't love the thunk or the inner lambda.  Maybe some syntax for helping to define these??
(define (sequence . hs)
   (thunk
     (define i  0)   

     (lambda (g e c)
       (if (= i (length hs))
         'done
         (let* ([h (list-ref hs i)]
                [ret (h g e c)]) 
           (cond 
             [(done? ret) 
              (begin 
                (set! i (add1 i))
                #f)] 
             [(noop? ret) #f] 
             [else ret]))))))




(define (times n h)

    (define done #f)
    (define to-go 0)

    (lambda (g e c)
      (define ret (h g e c))

      ;TODO: Rewrite this.  I have no idea if it is really correct.  
      (cond 
        [(done? ret) 
         (if (= to-go n) 
           'done
           (begin
             (set! to-go (add1 to-go))
             (unless (= to-go n)
               (set! h #f)) 
             (h g e c)))] 
        [(noop? ret) #f] 
        [else ret]))  
  )

 )
