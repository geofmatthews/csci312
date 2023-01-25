#lang plai

(require racket/trace)

(define (pow a b)
  (if (zero? b) 1
      (* a (pow a (- b 1)))))
(trace pow)
(pow 2 10)

(define (powtail a b)
  (powhelper a b 1))
(define (powhelper a b result)
  (if (zero? b) result
      (powhelper a (- b 1) (* a result))))
(trace powhelper)
(powtail 2 10)

(define (powlog a b)
  (cond ((zero? b) 1)
        ((odd? b) (* a (powlog a (- b 1))))
        (else (expt (powlog a (/ b 2)) 2))))
(trace powlog)
(powlog 2 10)
