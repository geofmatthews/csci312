#lang plai

(require racket/trace)

(define-type AE
  [num (n number?)]
  [add (left AE?) (right AE?)]
  [sub (left AE?) (right AE?)])

(define (parse sexpr)
  (cond [(number? sexpr) (num sexpr)]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (add (parse (second sexpr))
                     (parse (third sexpr)))]
           [(-) (sub (parse (second sexpr))
                     (parse (third sexpr)))])]))

(define (calc an-ae)
  (type-case AE an-ae
             [num (n) n]
             [add (l r) (+ (calc l) (calc r))]
             [sub (l r) (- (calc l) (calc r))]))

(trace calc parse)


(test (calc (parse '3)) 3)
(test (calc (parse '{+ 3 4})) 7)
(test (calc (parse '{+ {- 3 4} 7})) 6)