#lang plai

(require racket/trace)

(define-type WAE
  [num (n number?)]
  [add (left WAE?) (right WAE?)]
  [sub (left WAE?) (right WAE?)]
  [with (name symbol?) (named-exp WAE?) (body WAE?)]
  [id (name symbol?)])

;; parse : sexpr -> WAE

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
           )]))

;; subst : WAE symbol WAE ->WAE
(define (subst expr sub-id val)
  (type-case WAE expr
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
             [id (v) (if (symbol=? v sub-id) val expr)]))

;; calc : WAE ->number
(define (calc expr)
  (type-case WAE expr
             [num (n) n]
             [add (l r) (+ (calc l) (calc r))]
             [sub (l r) (- (calc l) (calc r))]
             [with (bound-id named-expr bound-body)
                   (calc (subst bound-body
                                bound-id
                                (num (calc named-expr))))]
             [id (v) (error 'calc "free identifier")]))
(define (calc-lazy expr)
  (type-case WAE expr
             [num (n) n]
             [add (l r) (+ (calc-lazy l) (calc-lazy r))]
             [sub (l r) (- (calc-lazy l) (calc-lazy r))]
             [with (bound-id named-expr bound-body)
                   (calc-lazy (subst bound-body
                                bound-id
                                named-expr))]
             [id (v) (error 'calc-lazy "free identifier")]))

(test (calc (parse '5)) 5)
(test (calc (parse '{+ 5 5})) 10)
(test (calc (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (calc (parse '{with {x 5} {+ x x}})) 10)
(test (calc (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
(test (calc (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc (parse '{with {x 5} {with {x x} x}})) 5)

(test (calc-lazy (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (calc-lazy (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (calc-lazy (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc-lazy (parse '{with {x 5} {with {x x} x}})) 5)


