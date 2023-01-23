#lang plai

(require racket/trace)

(define-type WAE
  [num (n number?)]
  [add (left WAE?) (right WAE?)]
  [with (name symbol?) (named-exp WAE?) (body WAE?)]
  [id (name symbol?)])

;; parse : sexpr -> WAE

(define (parse/k sexpr k)
  (cond [(number? sexpr) (k (num sexpr))]
        [(symbol? sexpr) (k (id sexpr))]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (parse/k (second sexpr)
                         (lambda (lhs)
                           (parse/k (third sexpr)
                                    (lambda (rhs)
                                      (k (add lhs rhs))))))]
           [(with) (parse/k (second (second sexpr))
                            (lambda (val)
                              (parse/k (third sexpr)
                                       (lambda (body)
                                         (k (with (first (second sexpr))
                                                  val
                                                  body))))))]
           )]))

;; subst : WAE symbol WAE ->WAE
(define (subst/k expr sub-id val k)
  (type-case WAE expr
             [num (n) (k expr)]
             [add (l r) (subst/k l sub-id val
                                 (lambda (lhs)
                                   (subst/k r sub-id val
                                            (lambda (rhs)
                                              (k (add lhs rhs))))))]
             [with (bound-id named-expr bound-body)
                   (if (symbol=? bound-id sub-id)
                       (subst/k named-expr sub-id val
                                (lambda (result)
                                  (k (with bound-id
                                           result
                                           bound-body))))
                       (subst/k named-expr sub-id val
                                (lambda (result)
                                  (subst/k bound-body sub-id val
                                           (lambda (body)
                                             (k (with bound-id
                                                      result
                                                      body)))))))]
             [id (v) (if (symbol=? v sub-id) (k val) (k expr))]))
;; calc : WAE ->number
(define (calc/k expr k)
  (type-case WAE expr
             [num (n) (k n)]
             [add (l r) 
                  (calc/k l
                          (lambda (lhs)
                            (calc/k r 
                                    (lambda (rhs)
                                      (k (+ lhs rhs))))))]
             [with (bound-id named-expr bound-body)
                   (calc/k named-expr
                           (lambda (expr)
                             (subst/k bound-body
                                      bound-id
                                      (num expr)
                                      (lambda (s)
                                        (calc/k s k)))))]
             [id (v) (error 'calc/k "free identifier")]))

(define (top-k top-level-func)
  (lambda (x)
    (begin (display "End of continuation for: ")
           (display top-level-func)
           (newline)
           x)))

(define (parse expr) (parse/k expr (top-k 'parse)))
(define (subst expr sub-id val) (subst/k exp sub-id val (top-k 'parse)))
(define (calc expr) (calc/k expr (top-k 'calc)))

(test (calc (parse '5)) 5)
(test (calc (parse '{+ 5 5})) 10)
(test (calc (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (calc (parse '{with {x 5} {+ x x}})) 10)
(test (calc (parse '{with {x {+ 5 5}} {with {y {+ x -3}} {+ y y}}})) 14)
(test (calc (parse '{with {x 5} {with {y {+ x -3}} {+ y y}}})) 4)
(test (calc (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (calc (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (calc (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (calc (parse '{with {x 5} {with {y x} y}})) 5)
(test (calc (parse '{with {x 5} {with {x x} x}})) 5)


