#lang plai

(define the-receiver (box 'dummy-value))

(define receiver-prompt (box 'dummy-value))

(define (web-display n)
  (printf "Web output:  ~a~n" n))

(define (web-read p)
  (begin
    (let/cc k
      (set-box! receiver-prompt p)
      (set-box! the-receiver k)
      (error 'web-read "run (resume) to enter number and simulate clicking Submit"))))

(define (resume)
  (begin
    (display (unbox receiver-prompt))
    ((unbox the-receiver) (read))))

(+ (web-read "First number: ")
   (web-read "Second number: "))
