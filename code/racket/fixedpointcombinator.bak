#lang racket

(define factorial-normal
  (lambda (n)
    (if (<= n 0)
        1
        (* n (factorial-normal (- n 1))))))
(factorial-normal 5)

(define factorial-passed
  (lambda (f n)
    (if (<= n 0)
        1
        (* n (f f (- n 1) )))))

(factorial-passed factorial-passed 5)

((lambda (f n)
   (if (<= n 0)
       1
       (* n (f f (- n 1) ))))
 (lambda (f n)
   (if (<= n 0)
       1
       (* n (f f (- n 1) ))))
 5)


(define factorial-generator
  (lambda (f)
    (lambda (n)
      (if (<= n 0)
          1
          (* n ((f f) (- n 1)))))))

((factorial-generator factorial-generator) 5)

(((lambda (f)
    (lambda (n)
      (if (<= n 0)
          1
          (* n ((f f) (- n 1))))))
  (lambda (f)
    (lambda (n)
      (if (<= n 0)
          1
          (* n ((f f) (- n 1)))))))
 5)

(define base
  (lambda (f)
    (lambda (n)
      (if (<= n 0)
          1
          (* n (f (- n 1)))))))

;; Find the fixed-piont of the base function:
;; (fixed-point base) == (base (fixed-point base))

(((lambda (recur)
    (base (lambda (arg)
            ((recur recur) arg))))
  (lambda (recur)
    (base (lambda (arg)
            ((recur recur) arg)))))
 5)


(define Y
  (lambda (base)
    ((lambda (recur)
       (base (lambda (arg)
               ( (recur recur) arg))))
     (lambda (recur)
       (base (lambda (arg)
               ( (recur recur) arg)))))))

(define fact
  (Y (lambda (f)
       (lambda (n)
         (if (<= n 0)
             1
             (* n (f (- n 1))))))))

(fact 5)

((Y (lambda (f)
      (lambda (n)
        (if (<= n 0)
            1
            (* n (f (- n 1)))))))
 5)

(((lambda (f)
    ((lambda (recur) (f (lambda (arg) ( (recur recur) arg))))
     (lambda (recur) (f (lambda (arg) ( (recur recur) arg))))))
  (lambda (f)
    (lambda (n)
      (if (<= n 0)
          1
          (* n (f (- n 1)))))))
 5)