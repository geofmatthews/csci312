#lang racket

(require racket/trace)

(define fact-tail
  (lambda (n accum)
    (if (zero? n) accum
        (fact-tail (- n 1) (* accum n)))))
(trace fact-tail)
(fact-tail 5 1)

(define fact-nontail
  (lambda (n)
    (if (zero? n) 1
        (* n (fact-nontail (- n 1))))))
(trace fact-nontail)
(fact-nontail 5)

(define fact-cps
  (lambda (n cont)
    (if (zero? n) (cont 1)
        (fact-cps (- n 1) (lambda (result) (cont (* n result)))))))
(trace fact-cps)
;(fact-cps 5 (lambda (n) n))

(define pow
  (lambda (n exp)
    (if (zero? exp) 1
        (* n (pow n (- exp 1))))))
(trace pow)
;(pow 3 5)
(define square (lambda (x) (* x x)))
(define pow-log
  (lambda (n exp)
    (cond ((zero? exp) 1)
          ((even? exp) (square (pow-log n (/ exp 2))))
          (else (* n (pow-log n (- exp 1)))))))
(trace pow-log)
;(pow-log 3 5)
;(pow 3 50)
;(pow-log 3 50)
(define pow-log-cps
  (lambda (n exp cont)
    (cond ((zero? exp) (cont 1))
          ((even? exp) (pow-log-cps n (/ exp 2) 
                                    (lambda (result) (cont (square result)))))
          (else (pow-log-cps n (- exp 1) 
                             (lambda (result) (cont (* n result))))))))
(trace pow-log-cps)
;(pow-log-cps 3 50 (lambda (x) x))
(define fib
  (lambda (n)
    (if (< n 2) 1
        (+ (fib (- n 1)) (fib (- n 2))))))
(trace fib)
(fib 5)
(define fib-cps
  (lambda (n cont)
    (if (< n 2) (cont 1)
        (fib-cps (- n 1)
                 (lambda (result1)
                   (fib-cps (- n 2)
                            (lambda (result2)
                              (cont (+ result1 result2)))))))))
(trace fib-cps)
(fib-cps 5 (lambda (x) x))

(define (myeven? n)
  (if (zero? n)
      #t
      (myodd? (- n 1))))
(define (myodd? n)
  (if (zero? n)
      #f
      (myeven? (- n 1))))

(define (myeven-cps? n cont)
  (if (zero? n)
      (cont #t)
      (myodd-cps? (- n 1) cont)))
(define (myodd-cps? n cont)
  (if (zero? n)
      (cont #f)
      (myeven-cps? (- n 1) cont)))

(let loop ((ls '(2 3 4 5)))
  (if (empty? ls) 'done
      (let ((x (car ls)))
        (display (list x (myeven? x) (myeven-cps? x (lambda (x) x))))
        (newline)
        (loop (cdr ls)))))

(define (foo n)
  (cond ((< n 3) (+ n 2))
        ((= 0 (remainder n 3))
         (+ 4 (foo (quotient n 3))))
        ((= 1 (remainder n 3))
         (+ (* 3 n) (foo (- n 1))))
        (else
         (+ n (foo (- n 1)) (foo (- n 2))))))

(define (foo-cps n cont)
  (cond ((< n 3) (cont (+ n 2)))
        ((= 0 (remainder n 3))
         (foo-cps (quotient n 3) (lambda (result) (cont (+ 4 result)))))
        ((= 1 (remainder n 3))
         (foo-cps (- n 1) (lambda (result) (cont (+ (* 3 n) result)))))
        (else
         (foo-cps (- n 1)
              (lambda (result1)
                (foo-cps (- n 2)
                     (lambda (result2)
                       (cont (+ n result1 result2)))))))))

(let loop ((ls '(2 3 4 5 6 7 8 9 10 11 12 13 14)))
  (if (empty? ls) 'done
      (let ((x (car ls)))
        (display (list x (foo x) (foo-cps x (lambda (x) x))))
        (newline)
        (loop (cdr ls)))))
      

(define (silly n)
  (cond ((< n 1) 9)
        ((even? n) (+ (silly (/ n 2)) 5))
        ((odd? n) (* (silly (/ (- n 1) 2)) (silly (- n 2))))))

(map silly '(0 1 2 3 4 5))

(define silly-cps
  (lambda (n cont)
    (cond ((< n 1) (cont 9))
          ((even? n) (silly-cps (/ n 2) (lambda (result)
                                      (cont (+ result 5)))))
          ((odd? n) (silly-cps (/ (- n 1) 2)
                           (lambda (result1)
                             (silly-cps (- n 2)
                                    (lambda (result2)
                                      (cont (* result1 result2))))))))))

(map (lambda (x) (silly-cps x (lambda (x) x))) '(0 1 2 3 4 5))

(define (funny n)
  (cond ((< n 1) 5)
        ((even? n) (* 2 (funny (funny (/ n 2)))))
        ((odd? n) (+ 2 (funny (- n 2))))))

(map funny '(0 1 2 3 4 5))

(define (funny/k n k)
  (cond ((< n 1) (k 5))
        ((even? n) (funny/k (/ n 2)
                            (lambda (result1)
                              (funny/k result1
                                     (lambda (result2)
                                       (k (* 2 result2)))))))
        ((odd? n) (funny/k (- n 2)
                           (lambda (result)
                             (k (+ 2 result)))))))

(map (lambda (x) (funny/k x (lambda (x) x))) '(0 1 2 3 4 5))