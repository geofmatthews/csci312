#lang plai

(define the-receiver (box 'dummy-value))

(define receiver-prompt (box 'dummy-value))

(define (web-display n)
  (printf "Web output:  ~a~n" n))

(define (web-read/k p k)
  (begin
    (set-box! receiver-prompt p)
    (set-box! the-receiver k)
    (error 'web-read/k "run (resume) to enter number and simulate clicking Submit")))

(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))


(define (get-one-temp/k c k)
  (web-read/k (format "Temperature in city  ~a:  " c)
              k))

(define (average ls) (/ (apply + ls) (length ls)))

(define (map/k f/k l k)
  (if (empty? l)
      (k empty)
      (f/k (first l)
           (lambda (v)
             (map/k f/k (rest l)
                    (lambda (v-rest)
                      (k (cons v v-rest))))))))


(map/k get-one-temp/k
       (list "Bangalore" "Budapest" "Houston" "Providence")
       (lambda (v)
         (web-display
          (average v))))