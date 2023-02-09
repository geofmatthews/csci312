#lang plai
;; Lazy cons
;(require (lib "scheme"))

(define-syntax lcons
  (syntax-rules ()
    ((lcons a b) (mcons a (lambda () b)))))

(define-syntax lcar
  (syntax-rules ()
    ((lcar x) (mcar x))))

(define-syntax lcdr
  (syntax-rules ()
    ((lcdr x) (cond ((procedure? (mcdr x))
                     (set-mcdr! x ((mcdr x)))
                     (mcdr x))
                    (else
                     (mcdr x))))))

(define nth
  (lambda (n stream)
    (cond ((zero? n) (lcar stream))
          (else (nth (- n 1) (lcdr stream))))))

(define front
  (lambda (n stream)
    (if (zero? n) (list '...)
        (cons (lcar stream) (front (- n 1) (lcdr stream) )))))

(define ones (lcons 1 ones))
(front 10 ones)

(define s+
  (lambda (s1 s2)
    (lcons (+ (lcar s1) (lcar s2))
           (s+ (lcdr s1) (lcdr s2)))))

(define ints (lcons 0 (s+ ints ones)))
(front 10 ints)

(define evens (s+ ints ints))
(front  10 evens)

(define odds (s+ ints (lcdr ints)))
(front 10 odds)

(define cumsum
  (lambda (stream)
    (let loop ((sum (lcar stream)) (stream (lcdr stream)))
      (lcons sum
             (loop (+ sum (lcar stream)) (lcdr stream))))))

(front 10 (cumsum ints))
(front 10 (cumsum odds))

(define fibs (lcons 1 (lcons 1 (s+ fibs (lcdr fibs)))))
(front 10 fibs)

(define keep-if
  (lambda (pred stream)
    (if (pred (lcar stream))
        (lcons (lcar stream) (keep-if pred (lcdr stream)))
        (keep-if pred (lcdr stream)))))

(define sieve
  (lambda (stream)
    (lcons
     (lcar stream)
     (sieve
      (keep-if
       (lambda (x)
         (not (zero? (remainder x (lcar stream)))))
       (lcdr stream))))))

(define primes (sieve (lcdr (lcdr ints))))
;(front 10 primes)