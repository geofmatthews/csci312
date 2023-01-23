#lang plai

(require racket/trace)

(define-type RCFAE
  [num (n number?)]
  [add (left RCFAE?) (right RCFAE?)]
  [mul (left RCFAE?) (right RCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body RCFAE?)]
  [app (fun-expr RCFAE?) (arg-expr RCFAE?)]
  [if0 (condition RCFAE?) (then RCFAE?) (else RCFAE?)]
  [rec (bound-id symbol?) (named-expr RCFAE?) (bound-body RCFAE?)]
  )

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

;; parse : sexpr -> WAE

(define (parse sexpr)
  (cond [(number? sexpr) (num sexpr)]
        [(symbol? sexpr) (id sexpr)]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (add (parse (second sexpr))
                     (parse (third sexpr)))]
           [(*) (mul (parse (second sexpr))
                     (parse (third sexpr)))]
           [(fun) (fun (first (second sexpr))
                       (parse (third sexpr)))]
           [(if0) (if0 (parse (second sexpr))
                       (parse (third sexpr))
                       (parse (fourth sexpr)))]
           [(rec) (rec (first (second sexpr))
                       (parse (second (second sexpr)))
                       (parse (third sexpr)))]
           [else (app (parse (first sexpr))
                      (parse (second sexpr)))]
           )]))

;; wparse : sexpr -> WAE
;; Adds "with-removal" to the parse

(define (wparse sexpr)
  (parse (preparse sexpr)))

(define-type RCFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body RCFAE?)
            (env Env?)])

(define (boxed-RCFAE-Value? v)
  (and (box? v)
       (RCFAE-Value? (unbox v))))

(define-type Env
  [mtSub]
  [aSub (name symbol?) (value RCFAE-Value?) (env Env?)]
  [aRecSub (name symbol?)
           (value boxed-RCFAE-Value?)
           (env Env?)]
  )

;; lookup : symbol Env -> RCFAE-Value
(define (lookup name env)
  (type-case Env env
             [mtSub () (error 'lookup "no binding for identifier")]
             [aSub (bound-name bound-value rest-env)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-env))]
             [aRecSub (bound-name boxed-bound-value rest-env)
                      (if (symbol=? bound-name name)
                          (unbox boxed-bound-value)
                          (lookup name rest-env))]
             ))

;; cyclically-bind-and-interp : symbol RCFAE env! env
(define (cyclically-bind-and-interp bound-id named-expr env)
  (local ([define value-holder (box (numV 1729))]
          [define new-env (aRecSub bound-id value-holder env)]
          [define named-expr-val (interp named-expr new-env)])
    (begin
      (set-box! value-holder named-expr-val)
      new-env)))

(define (op-args op RCFAEv1 RCFAEv2)
  (type-case RCFAE-Value RCFAEv1
             [numV (n) (numV (op n
                          (type-case RCFAE-Value RCFAEv2
                                     [numV (m) m]
                                     [else (error 'add-args "Adding non-numbers")])))]
             [else (error 'add-args "Adding non-numbers")]))

(define (zero-val? RCFAEv)
  (type-case RCFAE-Value RCFAEv
             [numV (n) (zero? n)]
             [else #f]))

;; interp : FWAE Env -> number
(define (interp expr env)
  (type-case RCFAE expr
             [num (n) (numV n)]
             [add (l r) (op-args + (interp l env) (interp r env))]
             [mul (l r) (op-args * (interp l env) (interp r env))]
             [id (v) (lookup v env)]
             [fun (bound-id bound-body)
                  (closureV bound-id bound-body env)]
             [if0 (condition thenpart elsepart)
                  (if (zero-val? (interp condition env))
                      (interp thenpart env)
                      (interp elsepart env))]
             [app (fun-expr arg-expr)
                  (local ([define fun-val (interp fun-expr env)])
                    (interp (closureV-body fun-val)
                            (aSub (closureV-param fun-val)
                                  (interp arg-expr env)
                                  (closureV-env fun-val) )))]
             [rec (bound-id named-expr bound-body)
               (interp bound-body
                       (cyclically-bind-and-interp bound-id
                                                   named-expr
                                                   env))]))
             

;; Check preparser results:
(wparse '(with (x 3)
               (with (f (fun (y) (+ x y)))
                     (with (x 5)
                           (f 4)))))

;; Tests
(test
 (interp (wparse '(with (x 3) x))
         (mtSub))
 (numV 3))

(test
 (interp (wparse '(with (x 3)
                        (with (f (fun (y) (+ x y)))
                              (with (x 5)
                                    (f 4)))))
         (mtSub))
 (numV 7))

(test
 (interp (wparse '(if0 1 2 3))
         (mtSub))
 (numV 3))

(test 
 (interp (wparse '(with (x 10)
                        (with (f (fun (y) (if0 y 99 (+ y y))))
                              (if0 x 22 (f x)))))
         (mtSub))
 (numV 20 ))

(test 
 (interp
  (wparse
   '(rec (f (fun (n) (if0 n 1 2)))
      (f 2)))
  (mtSub))
 (numV 2))

(test
 (interp
  (wparse
   '(rec (f (fun (n) (if0 n 0 (+ n (f (+ n -1))))))
      (f 5)))
  (mtSub))
 (numV 15))

                 
(test
 (interp
  (wparse
   '(rec (f (fun (n) (if0 n 1 (* n (f (+ n -1))))))
      (f 5)))
  (mtSub))
 (numV 120))

                         
