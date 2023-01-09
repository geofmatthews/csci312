#lang plai

(require rackunit
         "arithmetic.rkt")

;; These are provided by plai:
(test (slow+ 4 5) 9)
(test (slow* 4 5) 21)

;; These by rackunit:
(check-equal? (slow+ 4 5) 9)
(check-equal? (slow* 4 5) 21)

