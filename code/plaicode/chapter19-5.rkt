#lang plai

(define route-producer
  (local ([define resume (box false)])
    (lambda (real-send)
      (local ([define send (lambda (value-to-send)
                             (let/cc k
                               (begin
                                 (set-box! resume k)
                                 (real-send value-to-send))))])
        (if (unbox resume)
            ((unbox resume) 'dummy)
            (begin
              (send 'providence)
              (send 'houston)
              (send 'bangalore)))))))

(list
  (let/cc k (route-producer k))
  (let/cc k (route-producer k))
  (let/cc k (route-producer k))
)
;(define get call/cc)
