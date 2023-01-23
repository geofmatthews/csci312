#lang racket
(require racket/gui)

(define frame
  (make-object frame% "path finding"))

(define red-pen (make-object pen%
                  (make-object color% 255 0 0) 2 'solid))

(define blue-brush (make-object brush%
                     (make-object color% 0 0 255) 'solid))

(define my-canvas%
  (class canvas%
    (override on-event on-paint)
    (field (x 100) (y 100)) 
    (define on-event
      (lambda (event)
        (when (send event button-down?) ; mouse button event?
          (set! x (send event get-x))
          (set! y (send event get-y))
          (send this refresh))))
    (define on-paint
      (lambda ()
        (let ((dc (send this get-dc)))
          (send dc set-pen red-pen)
          (send dc set-brush blue-brush)
          (send dc draw-polygon
                (map (lambda (x y) (make-object point% x y))
                     (list x (+ x 50) (+ x 50) x)
                     (list y y (+ y 50) (+ y 50)))))))
    (super-instantiate ())
    ))
    

(define canvas
  (make-object my-canvas% frame))

(send canvas min-width 800)
(send canvas min-height 700)

(send canvas stretchable-width #f)
(send canvas stretchable-height #f)

(send frame show #t)


