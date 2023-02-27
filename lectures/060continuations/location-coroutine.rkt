(define-macro coroutine
  (lambda (x . body)
    `(letrec ((+local-control-state
               (lambda (,x) ,@body))
              (resume
               (lambda (c v)
                 (call/cc
                  (lambda (k)
                    (set! +local-control-state k)
                    (c v))))))
       (lambda (v)
         (+local-control-state v)))))

(define *rain-prob* 0.4)
(define *max-num-walks* (* 365 2 5)) ;2 walks/day for 5 years

(define make-location-cor
  (lambda (other-location-cor manager-cor)
    (coroutine v
      (let ((num-umbrellas 1))
        (let loop ((umbrella? (car v))
                   (walks-so-far (cadr v)))
          (when umbrella?
            (set! num-umbrellas (+ num-umbrellas 1)))
          (cond ((>= walks-so-far *max-num-walks*)
                 (resume manager-cor walks-so-far))
                ((< (random) *rain-prob*)
                 (cond ((> num-umbrellas 0)
                        (set! num-umbrellas
                          (- num-umbrellas 1))
                        (apply loop
                          (resume other-location-cor
                                  (list #t
                                        (+ walks-so-far 1)))))
                       (else
                         (apply loop
                           (resume manager-cor walks-so-far)))))
                (else
                  (apply loop
                    (resume other-location-cor
                            (list #f (+ walks-so-far 1)))))))))))

(define make-manager-cor
  (lambda (home-cor)
    (coroutine dummy-init-arg
      (resume home-cor (list #f 0)))))

(define umbrella-trial
  (lambda (rain-prob)
    (lambda ()
      (when (number? rain-prob) (set! *rain-prob* rain-prob))

(letrec ((home-cor (make-location-cor
                     (lambda (v) (office-cor v))
                     (lambda (v) (manager-cor v))))
         (office-cor (make-location-cor
                       (lambda (v) (home-cor v))
                       (lambda (v) (manager-cor v))))
         (manager-cor (make-manager-cor
                        (lambda (v) (home-cor v)))))
  
   (manager-cor 'start-the-ball-rolling)
   )
      )))

((umbrella-trial 0.4))