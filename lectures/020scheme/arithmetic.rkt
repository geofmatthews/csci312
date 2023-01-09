#lang plai

(define slow+
  (lambda (a b)
    (if (zero? a) b
        (slow+ (sub1 a) (add1 b)))))

(define slow*
  (lambda (a b)
    (if (zero? a) 0
        (slow+ b (slow* (sub1 a) b)))))

