#lang plai

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)]
  [if0 (condition F1WAE?) (then F1WAE?) (else F1WAE?)])

(define-type FunDef
  [fundef (fun-name symbol?)
          (arg-name symbol?)
          (body F1WAE?)])

(define (parse sexpr)
  (cond [(number? sexpr) (num sexpr)]
        [(symbol? sexpr) (id sexpr)]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (add (parse (second sexpr))
                     (parse (third sexpr)))]
           [(-) (sub (parse (second sexpr))
                     (parse (third sexpr)))]
           [(with) (with (first (second sexpr))
                         (parse (second (second sexpr)))
                         (parse (third sexpr)))]
           [(if0) (if0 (parse (second sexpr))
                       (parse (third sexpr))
                       (parse (fourth sexpr)))]
           [else (app (first sexpr) (parse (second sexpr)))]
           )])) 

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value number?) (ds DefrdSub?)])

;; lookup-fundef : symbol listof(FunDef) −! FunDef
(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))

;; lookup : symbol DefrdSub -> F1WAE
(define (lookup name ds)
  (type-case DefrdSub ds
             [mtSub () (error 'lookup "no binding for identifier")]
             [aSub (bound-name bound-value rest-ds)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-ds))]))

;; interp : F1WAE listof(fundef) DefrdSub! number
(define (interp expr fun-defs ds)
  (type-case F1WAE expr
             [num (n) n]
             [add (l r) (+ (interp l fun-defs ds) (interp r fun-defs ds))]
             [sub (l r) (- (interp l fun-defs ds) (interp r fun-defs ds))]
             [with (bound-id named-expr bound-body)
                   (interp bound-body
                           fun-defs
                           (aSub bound-id
                                 (interp named-expr
                                         fun-defs
                                         ds)
                                 ds))]
             [id (v) (lookup v ds)]
             [if0 (condition thenpart elsepart)
                  (if (zero? (interp condition fun-defs ds))
                      (interp thenpart fun-defs ds)
                      (interp elsepart fun-defs ds))]
             [app (fun-name arg-expr)
                  (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
                    (interp (fundef-body the-fun-def )
                            fun-defs
                            (aSub (fundef-arg-name the-fun-def )
                                  (interp arg-expr fun-defs ds)
                                  (mtSub))))]))

(test (interp (parse '{f 5})
              (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                    (fundef 'g 'm (sub (id 'm) (num 1))))
              (mtSub))
      9)

(test (interp (parse '(+ (f 3) (g (+ 2 (f 4)))))
              (list (fundef 'f 'n (add (num 10) (id 'n)))
                    (fundef 'g 'n (add (id 'n) (id 'n))))
              (mtSub))
      45)

(test (interp (parse '(even 13))
              (list (fundef 'odd 'n 
                            (if0 (id 'n) 
                                 (num 0)
                                 (app 'even (sub (id 'n) (num 1)))))
                    (fundef 'even 'n
                            (if0 (id 'n)
                                 (num 1)
                                 (app 'odd (sub (id 'n) (num 1)))))
                    )
              (mtSub)
              )
      0)

(test (interp (parse '(fib 9))
              (list (fundef 'fib 'n
                            (if0 (id 'n)
                                 (num 0)
                                 (if0 (sub (id 'n) (num 1))
                                      (num 1)
                                      (add (app 'fib (sub (id 'n) (num 1)))
                                           (app 'fib (sub (id 'n) (num 2))))))))
              (mtSub)
              )
      34)
