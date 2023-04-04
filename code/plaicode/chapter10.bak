#lang plai

(define-type RCFAE
  [num (n number?)]
  [add (lhs RCFAE?) (rhs RCFAE?)]
  [mult (lhs RCFAE?) (rhs RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
  [if0 (text-expr RCFAE?)
       (true-expr RCFAE?)
       (false-expr RCFAE?)]
  [rec (bound-id symbol?)
    (named-expr RCFAE?)
    (bound-body RCFAE?)]
  )



(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])


(define (num+ lhs rhs)
  (numV (+ (numV-n lhs) (numV-n rhs))))

(define (num* lhs rhs)
  (numV (* (numV-n lhs) (numV-n rhs))))

(define (num-zero? num)
  (= 0 (numV-n num)))

(define (boxed-RCFAE-Value? v)
  (and (box? v)
       (RCFAE-Value? (unbox v))))
(define-type Env
  [mtSub]
  [aSub (name symbol?)
        (value RCFAE-Value?)
        (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAE-Value?)
           (env Env?)])
;; lookup : symbol env! RCFAE-Value
(define (lookup name env)
  (type-case Env env
    [mtSub ()
           (error name "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]
    [aRecSub (bound-name boxed-bound-value rest-env)
             (if (symbol=? bound-name name)
                 (unbox boxed-bound-value)
                 (lookup name rest-env))]))
;; cyclically-bind-and-interp : symbol RCFAE env! env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))


;; interp : RCFAE env! RCFAE-Value
(define (interp expr env)
  (type-case RCFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [mult (l r) (num* (interp l env) (interp r env))]
    [if0 (test truth falsity)
         (if (num-zero? (interp test env))
             (interp truth env)
             (interp falsity env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp arg-expr env)
                         (closureV-env fun-val))))]
    [rec (bound-id named-expr bound-body)
      (interp bound-body
              (cyclically-bind-and-interp bound-id
                                          named-expr
                                          env))]))

(test (interp (num 3) (mtSub))
      (numV 3))
(test (interp (add (num 3) (num 3)) (mtSub))
      (numV 6))
(test (interp (mult (num 3)
                    (add (num 3) (num 3)))
              (mtSub))
      (numV (* 3 (+ 3 3))))

(test (interp
       (app (fun 'x (add (id 'x) (id 'x)))
            (num 3))
       (mtSub))
      (numV 6))


(test (interp
       (rec 'fac
         (fun 'n
              (if0 (id 'n) (num 1)
                   (mult (id 'n) (app (id 'fac)
                                      (add (id 'n)
                                           (num -1))))))
         (app (id 'fac) (num 5)))
       (mtSub))
      (numV 120))
