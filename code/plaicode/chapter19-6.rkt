#lang racket
(define (general-producer body)
(local ([define resume (box false)])
(lambda (real-send)
(local ([define send-to (box real-send)]
[define send (lambda (value-to-send)
(set-box! send-to
(let/cc k
(begin
(set-box! resume k)
((unbox send-to) value-to-send)))))])