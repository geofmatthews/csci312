#lang plai
(define-type Store
  [mtSto]
  [aSto (location number?)
        (value VCFAE-Value?)
        (store Store?)])

(define-type Value×Store
  [v×s (value VCFAE-Value?) (store Store?)])

(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value number?)
        (env Env?)])

(define-type VCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body VCFAE?)
            (env Env?)]
  [reference (n number?)])

(define (num+ l r)
  (numV (+ (numV-n l) (numV-n r))))
(define (num-zero? n)
  (zero? (num-n n)))
(define-type VCFAE
  (num (n number?))
  (add (l VCFAE?) (r VCFAE?))
  (id (v symbol?))
  [fun (id symbol?) (body VCFAE?)]
  [with (id symbol?) (exp VCFAE?) (body VCFAE?)]
  [app (fun VCFAE?) (arg VCFAE?)]
  [if0 (t VCFAE?) (then VCFAE?) (else VCFAE?)]
  [assign (id symbol?) (val VCFAE?)]
  [seqn (exp1 VCFAE?) (exp2 VCFAE?)]
  [ref (id symbol?)]
  )
;; env-lookup : symbol Env! location
(define (env-lookup name env)
  (type-case Env env
    [mtSub () (error 'env-lookup "no binding for identifier")]
    [aSub (bound-name bound-location rest-env)
          (if (symbol=? bound-name name)
              (if (reference? bound-location)
                  (reference-n bound-location)
                  bound-location)
              (env-lookup name rest-env))]))
;; store-lookup : location Store !BCFAE-Value
(define (store-lookup loc-index sto)
  (type-case Store sto
    [mtSto () (error'’store-lookup "no value at location")]
    [aSto (location value rest-store)
          (if (= location loc-index)
              value
              (store-lookup loc-index rest-store))]))
(define next-location
  (local ([define last-loc (box 999)])
    (lambda (store)
      (begin
        (set-box! last-loc (+ 1 (unbox last-loc)))
        (unbox last-loc)))))
;; interp : VCFAE Env Store !Value×Store
(define (interp expr env store)
  (type-case VCFAE expr
    [num (n) (v×s (numV n) store)]
    [add (l r)
         (type-case Value×Store (interp l env store)
           [v×s (l-value l-store)
                (type-case Value×Store (interp r env l-store)
                  [v×s (r-value r-store)
                       (v×s (num+ l-value r-value)
                            r-store)])])]
    [id (v) (v×s (store-lookup (env-lookup v env) store) store)]
    [fun (bound-id bound-body)
         (v×s (closureV bound-id bound-body env) store)]
    [app (fun-expr arg-expr)
         (type-case Value×Store (interp fun-expr env store)
           [v×s (fun-value fun-store)
                (type-case Value×Store (interp arg-expr env fun-store)
                  [v×s (arg-value arg-store)
                       (cond ((reference? arg-value)
                              (let ((loc (reference-n arg-value)))
                                (interp (closureV-body fun-value)
                                        (aSub (closureV-param fun-value)
                                              loc
                                              (closureV-env fun-value))
                                        arg-store)))
                             (else
                              (let ((new-loc (next-location arg-store)))
                                (interp (closureV-body fun-value)
                                        (aSub (closureV-param fun-value)
                                              new-loc
                                              (closureV-env fun-value))
                                        (aSto new-loc
                                              arg-value
                                              arg-store)))))])])]
    [with (var val body)
          (type-case Value×Store (interp val env store)
            [v×s (val-value val-store)
                 (local ((define new-loc (next-location val-store)))
                   (interp body
                           (aSub var
                                 new-loc
                                 env)
                           (aSto new-loc
                                 val-value
                                 val-store)))])]
    [if0 (test truth falsity)
         (type-case Value×Store (interp test env store)
           [v×s (test-value test-store)
                (if (num-zero? test-value)
                    (interp truth env test-store)
                    (interp falsity env test-store))])]
    [assign (var value)
            (type-case Value×Store (interp value env store)
              [v×s (value-value value-store)
                   (local ([define the-loc (env-lookup var env)])
                     (v×s value-value
                          (aSto the-loc value-value value-store)))])]
    [seqn (e1 e2)
          (type-case Value×Store (interp e1 env store)
            [v×s (e1-value e1-store)
                 (interp e2 env e1-store)])]
    [ref (var)
         [v×s (reference (env-lookup var env)) store]]
    ))

(test
 (type-case Value×Store
   (interp (app (fun 'x (seqn (assign 'x (add (id 'x) (num 1)))
                              (add (id 'x) (num 1))))
                (num 2))
           (mtSub)
           (mtSto))
   [v×s (v s) v])
 (numV 4))
   
(test
 (type-case Value×Store
   (interp (with 'x
                 (num 2)
                 (seqn (assign 'x (add (id 'x) (num 1)))
                       (add (id 'x) (num 1))))
           (mtSub)
           (mtSto))
   [v×s (v s) v])
 (numV 4))


(test
 (type-case Value×Store 
   (interp
    {with 'swap {fun 'x
                     {fun 'y
                          {with 'z (id 'x)
                                {seqn {assign 'x (id 'y)}
                                      {assign 'y (id 'z)}}}}}
          {with 'a (num 3)
                {with 'b (num 2)
                      {seqn {app {app (id 'swap) {ref 'a}} {ref 'b}}
                            (id 'b)}}}}
    (mtSub)
    (mtSto))
   [v×s (v s) v])
 (numV 3))

(test
 (type-case Value×Store
   (interp
    (with 'x (num 44)
          (with 'f (fun 'y (id 'y))
                (app (id 'f) (ref 'x))))
    (mtSub)
    (mtSto))
   [v×s (v s) v])
 (numV 44))

(interp
 (with 'x (num 33) (ref 'x))
 (mtSub)
 (mtSto))

(interp
 (app (fun 'x (ref 'x)) (num 3))
 (mtSub)
 (mtSto))

(test
 (type-case Value×Store
   (interp
    (app
     (fun 'x
          (app (fun 'f
                    (app (id 'f) (ref 'x)))
               (fun 'y (id 'y))))
          (num 44))
    (mtSub)
    (mtSto))
   [v×s (v s) v])
 (numV 44))

(test
 (type-case Value×Store 
   (interp
    (app (fun 'swap
              (app (fun 'a
                        (app (fun 'b
                                  (seqn (app (app (id 'swap)
                                                  (ref 'a))
                                             (ref 'b))
                                        (id 'b)))
                             (num 2)))
                   (num 3)))
         (fun 'x
              (fun 'y
                   (app (fun 'z
                             (seqn (assign 'x (id 'y))
                                   (assign 'y (id 'z))))
                        (id 'x)))))
    (mtSub)
    (mtSto))
   [v×s (v s) v])
 (numV 3))

   
