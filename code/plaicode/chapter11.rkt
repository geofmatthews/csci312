(require plai)

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])
(define (Env? x)
  (procedure? x))
(define (mtSub)
  (lambda (name)
    (error ’lookup "no binding for identifier")))
(define (aSub bound-name bound-value env)
  (lambda (want-name)
    (cond
      [(symbol=? want-name bound-name) bound-value]
      [else (lookup want-name env)])))
(define (cyclically-bind-and-interp bound-name named-expr env)
  (local ([define rec-ext-env
            (lambda (want-name)
              (cond
                [(symbol=? want-name bound-name)
                 (closureV (fun-param named-expr)
                           (fun-body named-expr)
                           rec-ext-env)]
                [else (lookup want-name env)]))])
    rec-ext-env))
(define (lookup name env)
  (env name))

(define (number-or-procedure? v)
  (or (number? v)
      (procedure? v)))
(define-type Env
  [mtSub]
  [aSub (name symbol?) (value number-or-procedure?) (env Env?)])
(define-type FAE-Value
  [numV (n number?)]
  [closureV (p procedure?)])

;; interp : FAE Env!FAE-Value
(define (interp expr env)
  (type-case FAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV (lambda (arg-val)
                     (interp bound-body
                             (aSub bound-id arg-val env))))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           ((closureV-p fun-val)
            arg-val))]))
;; lookup : symbol Env !number-or-procedure
(define (lookup name env)
  (type-case Env env
    [mtSub () (error ’lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))

;; interp : FAE Env!number-or-procedure
(define (interp expr env)
  (type-case FAE expr
    [num (n) n]
    [add (l r) (+ (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (lambda (arg-val)
           (interp bound-body
                   (aSub bound-id arg-val env)))]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr env)]
                 [define arg-val (interp arg-expr env)])
           (fun-val arg-val))]))