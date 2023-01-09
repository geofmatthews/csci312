#lang plai

(define foo
  (lambda (x) (values x (* x x))))

(define bar
  (lambda (x) (values x (* x x x))))

(let-values (((a b) (foo 3))
             ((c d) (bar 3)))
  (list a b d))