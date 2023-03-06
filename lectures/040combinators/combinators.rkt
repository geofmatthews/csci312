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
;; Since this is a pure function, we
;; can use it without naming it:
((lambda (f n)
   (if (<= n 0)
       1
       (* n (f f (- n 1) ))))
 (lambda (f n)
   (if (<= n 0)
       1
       (* n (f f (- n 1) ))))
 5)


(define factorial-generator ;; Curried
  (lambda (f)
    (lambda (n)
      (if (<= n 0)
          1
          (* n ((f f) (- n 1)))))))

((factorial-generator factorial-generator) 5)

;; Use without naming:
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

(((lambda (f)
    (base (lambda (n)
            ((f f) n))))
  (lambda (f)
    (base (lambda (n)
            ((f f) n)))))
 5)


(define Y
  (lambda (base)
    ((lambda (f)
       (base (lambda (n)
               ( (f f) n))))
     (lambda (f)
       (base (lambda (n)
               ( (f f) n)))))))

(define fact-base (Y base))
(fact-base 5)

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
