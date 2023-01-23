#lang plai

(require racket/trace)

(define-type FAE
  [num (n number?)]
  [add (left FAE?) (right FAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (intype FAE-type?) (body FAE?)]
  [app (fun-expr FAE?) (arg-expr FAE?)]
  [if0 (condition FAE?) (then FAE?) (else FAE?)])

(define-type FAE-type
  [numtype]
  [funtype (in FAE-type?) (out FAE-type?)]
  )

;; parse : sexpr -> FAE
(define (parse sexpr)
  (cond [(number? sexpr) (num sexpr)]
        [(symbol? sexpr) (id sexpr)]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (add (parse (second sexpr))
                     (parse (third sexpr)))]
           [(fun) (fun (first (second sexpr))
                       (parse-type (third (second sexpr)))
                       (parse (third sexpr)))]
           [(if0) (if0 (parse (second sexpr))
                       (parse (third sexpr))
                       (parse (fourth sexpr)))]
           [else (app (parse (first sexpr))
                      (parse (second sexpr)))]
           )]))

;; parse-type : sexpr -> FAE-type
(define (parse-type sexpr)
  (cond [(equal? sexpr 'num) (numtype)]
        [else (funtype (parse-type (first sexpr))
                       (parse-type (third sexpr)))]))
(define (unparse-type type)
  (type-case FAE-type type
             [numtype () 'num]
             [funtype (l r) (list (unparse-type l) '-> (unparse-type r))]))

(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value FAE-type?) (env Environment?)])

;; lookup : symbol Environment -> FAE-Type
(define (lookup name env)
  (type-case Environment env
             [mtSub () (error 'lookup (format "Type error: no binding for identifier ~s" name))]
             [aSub (bound-name bound-value rest-env)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-env))]))

;; type-of : FAE Environment -> number
(define (type-of expr env)
  (type-case FAE expr
             [num (n) (numtype)]
             [add (l r) 
                  (type-case FAE-type (type-of l env)
                             [numtype () 
                                      (type-case FAE-type (type-of r env)
                                                 [numtype () (numtype)]
                                                 [else (error "Type error: non-number to addition")])]
                             [else (error "Type error: non-number to addition")])]
             [id (v) (lookup v env)]
             [fun (bound-id in-type bound-body)
                  (funtype in-type (type-of bound-body
                                           (aSub bound-id
                                                 in-type
                                                 env)))]
             [if0 (condition thenpart elsepart)
                  (type-case FAE-type (type-of condition env)
                             [numtype ()
                                      (let ((t1 (type-of thenpart env))
                                            (t2 (type-of elsepart env)))
                                        (if (equal? t1 t2)
                                            t1
                                            (error "Type error: Conditional branches don't match")))]
                             [else (error "Type error: Condition not a number")]
                             )]
             [app (fun-expr arg-expr)
                  (type-case FAE-type (type-of fun-expr env)
                             [funtype (intype outtype)
                                      (if (equal? intype (type-of arg-expr env))
                                          outtype
                                          (error "Type error: bad app"))]
                             [else (error (format "Type error: non function in app ~s" fun-expr))])]
             ))

(for-each (lambda (x) (display (list x '====> (unparse-type (type-of (parse x) (mtSub))))) (newline))
     '(
       (fun (x : num) x)
       ((fun (x : num) x) 9)
       (fun (x : (num -> num)) (x 10))
       (fun (x : (num -> (num -> num))) (x 10))
       ((fun (x : ((num -> num) -> (num -> num))) (fun (y : (num -> num)) (y 10)))
        (fun (x : (num -> num)) (fun (y : num) (+ (x y) (x y)))))
       ))
#|                                            
;; Tests
(test
 (type-of
  (parse '(+ 4 10))
  (mtSub))
 (numtype))

(test 
 (type-of
  (parse '(fun (x : num) x))
  (mtSub))
 (funtype (numtype) (numtype)))

(test
 (type-of
  (parse
   '((fun (f : (num -> num)) (f 3))
     (fun (x : num) (+ x x))))
  (mtSub))
 (numtype))

(test
 (type-of
  (parse
   '(if0 3 4 5))
  (mtSub))
 (numtype))

(test
 (type-of
  (parse
   '((fun (f : (num -> num))
          (if0 (f 3) (f 4) (f 5)))
     (fun (x : num) (+ x x))))
  (mtSub))
 (numtype))

(test
 (type-of
  (parse
   '(fun (a : (num -> num)) (fun (b : ((num -> num) -> num)) (b a))))
  (mtSub))
 (parse-type '((num -> num) -> (((num -> num) -> num) -> num))))

(test
 (type-of
  (parse
   '(fun (a : num) (fun (b : (num -> num)) (b a))))
  (mtSub))
 (parse-type '(num -> ((num -> num) -> num))))

(type-of
 (parse
  '((fun (f : (num -> num)) (f 10))
    (fun (i : num) (f i))))
  (mtSub))
|#
;; Type error:
;;(type-of (parse '(+ 3 (fun (x : num) x))) (mtSub))
;;(type-of (parse '(if0 1 2 (fun (x : num) x))) (mtSub))
;; (type-of (parse '(fun (a : num) (fun (b : (num -> num)) (a b)))) (mtSub))