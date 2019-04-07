#lang racket

(provide log apply-handler apply-op compose-handlers for-ticks on-rule remove-self 
         g->g-to-handler 
         g->e-to-handler 
         g->c-to-handler 
         e->e-to-handler 
         e->c-to-handler 
         c->c-to-handler 
         handler-to-g->g
         handler-to-g->e 
         handler-to-g->c
         handler-to-e->e 
         handler-to-e->c 
         handler-to-c->c
         
         is-handler?)

(require "./crud.rkt"
         "./base.rkt")

(define (is-handler? h) 
  (and (procedure? h)
       (= 3 (procedure-arity h))))

;TODO: move to base
(define rule? (-> game? entity? component? boolean?))

;Lifting common function types to handlers
; Some kind of weird algebra...

  (define/contract (g->g-to-handler g->g)
     (-> (-> game? game?) game-handler?)              
     (lambda (g e c)
       (g->g g)) )

  (define/contract (g->e-to-handler g->e)
     (-> (-> game? entity?) entity-handler?)              
     (lambda (g e c)
       (g->e e)))

  (define/contract (g->c-to-handler g->c)
     (-> (-> game? component?) component-handler?)              
     (lambda (g e c)
       (g->c c)))

  (define/contract (e->e-to-handler e->e)
     (-> (-> entity? entity?) entity-handler?)              
     (lambda (g e c)
       (e->e e)))

  (define/contract (e->c-to-handler e->c)
     (-> (-> entity? component?) component-handler?)              
     (lambda (g e c)
       (e->c e)))

  (define/contract (c->c-to-handler c->c)
     (-> (-> component? component?) component-handler?)              
     (lambda (g e c)
       (c->c c)))


(define/contract (handler-to-g->g h)
     (-> game-handler? (-> game? game?))              
                 
     (lambda (g)
       (h g #f #f))) 

(define/contract (handler-to-g->e h)
     (-> entity-handler? (-> game? entity?))              
                 
     (lambda (g)
       (h g #f #f))) 

(define/contract (handler-to-g->c h)
     (-> component-handler? (-> game? component?))              
                 
     (lambda (g)
       (h g #f #f))) 



(define/contract (handler-to-e->e h)
     (-> entity-handler? (-> entity? entity?))              
                 
     (lambda (e)
       (h #f e #f))) 

(define/contract (handler-to-e->c h)
     (-> component-handler? (-> entity? component?))              
                 
     (lambda (e)
       (h #f e #f))) 

(define/contract (handler-to-c->c h)
     (-> component-handler? (-> component? component?))              
                 
     (lambda (c)
       (h #f #f c))) 


;HANDLERS


(define/contract (remove-self)
  (-> handler?)
  (lambda (g e c)
    (remove-component* e c)))

(define/contract (log msg)
  (-> string? handler?)
  (lambda (g e c)
    (displayln msg) 
    'noop))

(define/contract (on-rule r h)
  (-> rule? handler? handler?)

  (lambda (g e c)
    (if (r g e c)
        (h g e c)
        'noop)))


(define/contract (apply-handler h g e c)
  (-> handler? game? entity? component? operation?)

  (define op (h g e c))
  (apply-op op g e c))


(define/contract (compose-handlers . hs)
   (->* () () #:rest (listof handler-convertable?) handler?)

   (lambda (g e c) 
     (foldl (lambda (f g)
              (define new-e (get-entity* g e)) 
              (define new-c (get-component* e c)) 

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
    [(entity? o) (update-entity* g e o)]
    [(component? o) (update-entity* g e 
                                    (update-component* e c o))]
    [(noop? o) g]
    [(done? o) (update-entity* g e 
                                 (update-component* e c (component-done c)))]
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
