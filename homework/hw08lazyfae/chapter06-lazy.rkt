#lang plai

(require racket/trace)

(define-type CFAE/L
  [num (n number?)]
  [add (left CFAE/L?) (right CFAE/L?)]
  [id (name symbol?)]
  [fun (param symbol?) (body CFAE/L?)]
  [app (fun-expr CFAE/L?) (arg-expr CFAE/L?)])

;; preparse : sexpr -> sexpr
;; This changes (with (x e) b) into ((fun (x) b) e)

(define (preparse sexpr)
  (cond ((not (list? sexpr)) sexpr)
        ((case (first sexpr)
           [(with)
            (let ((bound-var (first (second sexpr)))
                  (bound-val (second (second sexpr)))
                  (body (third sexpr)))
              (let ((new-sexpr 
                     (list
                      (list 'fun (list bound-var) body)
                      bound-val)))
                (map preparse new-sexpr)))]
           [else
            (map preparse sexpr)]))))

;; parse : sexpr -> CFAE/L

(define (parse sexpr)
  (cond [(number? sexpr) (num sexpr)]
        [(symbol? sexpr) (id sexpr)]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (add (parse (second sexpr))
                     (parse (third sexpr)))]
           [(fun) (fun (first (second sexpr))
                       (parse (third sexpr)))]
           [else (app (parse (first sexpr))
                      (parse (second sexpr)))]
           )]))

;; wparse : sexpr -> CFAE/L
;; Adds "with-removal" to the parse

(define (wparse sexpr)
  (parse (preparse sexpr)))

(define-type CFAE/L-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body CFAE/L?)
            (env Environment?)]
  [exprV (expr CFAE/L?)
         (env Environment?)])

(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value CFAE/L-Value?) (env Environment?)])

;; lookup : symbol Environment -> CFAE/L-Value
(define (lookup name env)
  (type-case Environment env
    [mtSub () (error 'lookup
                     (format "no binding for identifier ~a" name))]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))


(define (add-args v1 v2)
  (numV (+ (numV-n (strict v1)) (numV-n (strict v2))))) 

;; interp : FWAE Environment -> number
(define (interp expr env)
  (type-case CFAE/L expr
    [num (n) (numV n)]
    [add (l r) (add-args (interp l env) (interp r env))]
    [id (v) (lookup v env)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body env)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (strict (interp fun-expr env))]
                 [define arg-val (exprV arg-expr env)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         arg-val
                         (closureV-env fun-val) )))]
    ))

(define (strict e)
  (type-case CFAE/L-Value e
    [exprV (expr env)
           (strict (interp expr env))]
    [else e]))

;; Tests
(test
 (strict
  (interp (wparse '(with (x 3) x))
          (mtSub)))
 (numV 3))

(test
 (strict
  (interp (wparse '(with (x 3)
                         (with (f (fun (y) (+ x y)))
                               (with (x 5)
                                     (f 4)))))
          (mtSub)))
 (numV 7))

(test
 (strict
  (interp (wparse '(with (f (undef x)) 4))
          (mtSub)))
 (numV 4))
