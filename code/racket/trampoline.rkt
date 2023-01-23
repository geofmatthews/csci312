#lang racket
(define trampoline
  (lambda (x)
    (if (procedure? x)
        (trampoline (x))
        x)))

(define f
  (lambda (x n)
    (display (list x n)) (newline)
    (lambda ()
      (if (zero? x) 
          n
          (f (- x 1) (* x n))))))

(trampoline (f 5 1))

(define f2
  (lambda (x)
    (display x) (newline)
    (lambda ()
      (if (zero? x)
          1
          (lambda ()
            (let ((result (trampoline (f2 (- x 1)))))
              (* x result)))))))

(trampoline (f2 5))