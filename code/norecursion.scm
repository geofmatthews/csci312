#lang racket

(define (f n)
  (if (< n 1)
      1
      (* n (f (- n 1)))))

(f 5)

(define (f2 g n)
  (if (< n 1)
      1
      (* n (g g (- n 1)))))

(f2 f2 5)

(define (f3 n) (f2 f2 n))

(f3 5)

((lambda (g n)
   (if (< n 1)
       1
       (* n (g g (- n 1)))))
 (lambda (g n)
   (if (< n 1)
       1
       (* n (g g (- n 1)))))
  5)
   
