#lang plai

(require racket/trace)

(define-type FAE
  [num (n number?)]
  [add (left FAE?) (right FAE?)]
  [mult (left FAE?) (right FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)]
  [if0 (condition FAE?) (then FAE?) (else FAE?)])

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

;; parse : sexpr -> FAE

(define (parse sexpr)
  (cond [(number? sexpr) (num sexpr)]
        [(symbol? sexpr) (id sexpr)]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (add (parse (second sexpr))
                     (parse (third sexpr)))]
           [(*) (mult (parse (second sexpr))
                     (parse (third sexpr)))]
           [(fun) (fun (first (second sexpr))
                       (parse (third sexpr)))]
           [(if0) (if0 (parse (second sexpr))
                       (parse (third sexpr))
                       (parse (fourth sexpr)))]
           [else (app (parse (first sexpr))
                      (parse (second sexpr)))]
           )]))

;; wparse : sexpr -> FAE
;; Adds "with-removal" to the parse

(define (wparse sexpr)
  (parse (preparse sexpr)))

(define-type FAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body FAE?)
            (env Environment?)])

(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value FAE-Value?) (env Environment?)])

;; lookup : symbol Environment -> FAE-Value
(define (lookup name env)
  (type-case Environment env
             [mtSub () (error 'lookup
                              (format "no binding for identifier ~a" name))]
             [aSub (bound-name bound-value rest-env)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-env))]))


(define (add-args faev1 faev2)
  (type-case FAE-Value faev1
             [numV (n) (numV (+ n
                          (type-case FAE-Value faev2
                                     [numV (m) m]
                                     [else (error 'add-args "Adding non-numbers")])))]
             [else (error 'add-args "Adding non-numbers")]))

(define (mult-args faev1 faev2)
  (type-case FAE-Value faev1
             [numV (n) (numV (* n
                          (type-case FAE-Value faev2
                                     [numV (m) m]
                                     [else (error 'add-args "Adding non-numbers")])))]
             [else (error 'add-args "Adding non-numbers")]))

(define (zero-val? faev)
  (type-case FAE-Value faev
             [numV (n) (zero? n)]
             [else #f]))

;; interp : FWAE Environment -> number
(define (interp expr env)
  (type-case FAE expr
             [num (n) (numV n)]
             [add (l r) (add-args (interp l env) (interp r env))]
             [mult (l r) (mult-args (interp l env) (interp r env))]
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
             ))

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

(require racket/trace)
(trace interp)
(interp (wparse '(with (x 10)
                        (with (f (fun (y) (if0 y 99 (+ y y))))
                              (if0 x 22 (f x)))))
         (mtSub))

(interp (wparse '(with (fac (fun (n)
                                 (if0 n
                                      1
                                      (* n (fac (+ n -1)))))
                            )
                       (fac 5)))
        (mtSub))

