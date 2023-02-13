#lang plai



(define-type CFAE/L
  [num (n number?)]
  [add (lhs CFAE/L?) (rhs CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])
(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Env?)]
  [exprV (expr CFAE/L?)
         (env Env?)])
(define-type Env
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (env Env?)])
;; lookup : symbol Env FAE-Value
(define (lookup name env)
  (type-case Env env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))

;; num+ : CFAE/L-Value CFAE/L-Value -! numV
(define (num+ n1 n2)
  (numV (+ (numV-n (strict n1)) (numV-n (strict n2)))))
;; num-zero? : CFAE/L-Value !boolean
(define (num-zero? n)
  (zero? (numV-n (strict n))))
;; strict : CFAE/L-Value ! CFAE/L-Value [excluding exprV]
(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (local ([define the-value (strict (interp expr env))])
             (begin
               (printf "Forcing exprV to ~a ~n" the-value)
               the-value))]
    [else e]))
;; interp : CFAE/L Env !CFAE/L-Value
(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (strict (interp fun-expr env))]
                 [define arg-val (exprV arg-expr env)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         arg-val
                         (closureV-env fun-val))))]))

(test (interp (add (num 3) (num 3)) (mtSub))
      (numV 6))

(test (interp
       (app (fun 'x (add (id 'x) (id 'x)))
           (num 3)) (mtSub))
      (numV 6))