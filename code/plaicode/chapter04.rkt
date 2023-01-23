#lang plai

(define-type F1WAE
  [num (n number?)]
  [add (lhs F1WAE?) (rhs F1WAE?)]
  [sub (lhs F1WAE?) (rhs F1WAE?)]
  [with (name symbol?) (named-expr F1WAE?) (body F1WAE?)]
  [id (name symbol?)]
  [app (fun-name symbol?) (arg F1WAE?)]
  [if0 (condition F1WAE?) (then F1WAE?) (else F1WAE?)]
  )

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

;; lookup-fundef : symbol listof(FunDef) −! FunDef
(define (lookup-fundef fun-name fundefs)
  (cond
    [(empty? fundefs) (error fun-name "function not found")]
    [else (if (symbol=? fun-name (fundef-fun-name (first fundefs)))
              (first fundefs)
              (lookup-fundef fun-name (rest fundefs)))]))
;; subst : F1WAE symbol F1WAE ! F1WAE
(define (subst expr sub-id val)
  (type-case F1WAE expr
             [num (n) expr]
             [add (l r) (add (subst l sub-id val)
                             (subst r sub-id val))]
             [sub (l r) (sub (subst l sub-id val)
                             (subst r sub-id val))]
             [with (bound-id named-expr bound-body)
                   (if (symbol=? bound-id sub-id)
                       (with bound-id
                             (subst named-expr sub-id val)
                             bound-body)
                       (with bound-id
                             (subst named-expr sub-id val)
                             (subst bound-body sub-id val)))]
             [id (v) (if (symbol=? v sub-id) val expr)]
             [app (fun-name arg-expr)
                  (app fun-name (subst arg-expr sub-id val))]
             [if0 (condition thenpart elsepart)
                  (if0 (subst condition sub-id val)
                       (subst thenpart sub-id val)
                       (subst elsepart sub-id val))]
             ))

;; interp : F1WAE listof(fundef)! number
(define (interp expr fun-defs)
  (type-case F1WAE expr
             [num (n) n]
             [add (l r) (+ (interp l fun-defs) (interp r fun-defs))]
             [sub (l r) (- (interp l fun-defs) (interp r fun-defs))]
             [with (bound-id named-expr bound-body)
                   (interp (subst bound-body
                                  bound-id
                                  (num (interp named-expr fun-defs)))
                           fun-defs)]
             [id (v) (error 'interp "free identifier")]
             [app (fun-name arg-expr)
                  (local ([define the-fun-def (lookup-fundef fun-name fun-defs)])
                    (interp (subst (fundef-body the-fun-def )
                                   (fundef-arg-name the-fun-def )
                                   (num (interp arg-expr fun-defs)))
                            fun-defs))]
             [if0 (condition thenpart elsepart)
                  (if (zero? (interp condition fun-defs))
                      (interp thenpart fun-defs)
                      (interp elsepart fun-defs))]
             ))

(test (interp (parse '{f 5})
              (list (fundef 'f 'n (app 'g (add (id 'n) (num 5))))
                    (fundef 'g 'm (sub (id 'm) (num 1)))))
      9)

(test (interp (parse '(+ (f 3) (g (+ 2 (f 4)))))
              (list (fundef 'f 'n (add (num 10) (id 'n)))
                    (fundef 'g 'n (add (id 'n) (id 'n)))))
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
              )
      34)
