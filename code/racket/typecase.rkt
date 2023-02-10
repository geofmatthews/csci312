#lang plai
(define-type point
  (oned (x number?))
  (twod (x number?) (y number?))
  (threed (x number?) (y number?) (z number?)))

(define p1 (oned 8))
(define p2 (twod 3 4))
(define p3 (threed 2 9 4))

(define (sum pt)
  (type-case point pt
    (oned (x) x)
    (twod (x y) (+ x y))
    (threed (x y z) (+ x y z))))

(define (add pt)
  (cond ((oned? pt) (oned-x pt))
        ((twod? pt) (+ (twod-x pt) (twod-y pt)))
        ((threed? pt) (+ (threed-x pt)
                         (threed-y pt)
                         (threed-z pt)))))

(sum p1)
(sum p2)
(sum p3)

(add p1)
(add p2)
(add p3)
