(define (stack)
  (let ((items '()))
    (lambda args
      (case (car args)
        ((pop) (let ((x (car items)))
                 (set! items (cdr items))
                 x))
        ((push) (set! items (cons (cadr args) items)))
        ((empty?) (empty? items))
        ((show) (display items) (newline))))))

(define (mypow n p)
  (cond ((zero? p) 1)
        ((odd? p) (* n (mypow n (- p 1))))
        (else (expt (mypow n (quotient p 2)) 2))))

(define (mypowstack n p)
  (let ((s (stack)))
    (let loop ((p p))
      (cond ((zero? p) 1)
            ((odd? p) (s 'push 'multiply)
                      (loop (- p 1)))
            (else (s 'push 'square)
                  (loop (quotient p 2)))))
    (let loop ((accumulator 1))
      (cond ((s 'empty?) accumulator)
            ((eq? (s 'pop) 'multiply)
             (loop (* accumulator n)))
            (else
             (loop (* accumulator accumulator)))))))

(define (mypow1 n p)

(let loop ((ls '(2 3 4 5 10 20)))
  (cond ((empty? ls) 'done)
        (else
         (let ((x (car ls)))
           (display (list x (mypow 2 x) (mypowstack 2 x)))
           (newline)
           (loop (cdr ls))))))
  