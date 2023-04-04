(define (myfoldl f item ls)
  (if (empty? ls) item
      (myfoldl f (f (car ls) item) (cdr ls))))

(foldl cons '() '(1 2 3 4))
(myfoldl cons '() '(1 2 3 4))

(define (sum ls)
  (foldl + 0 ls))
(sum '(1 2 3 4 5))
(define (product ls)
  (foldl * 1 ls))
(product '(1 2 3 4 5))
(define (count ls)
  (foldl (lambda (x y) (+ 1 y)) 0 ls))
(count '(1 2 3 4 5))

(define (merge . lsts)
  (foldl (lambda (x y) (append y x)) '() lsts))
(merge '(1 2 3) '(4 5 6) '(7 8 9))

(define (mymax ls)
  (foldl (lambda (x y) (if (> x y) x y)) (car ls) (cdr ls)))
(mymax '(1 5 4 7 2 9 8 6))

(define (mycompose funcs)
  (foldl (lambda (f g) (lambda (x) (f (g x)))) (lambda (x) x) funcs))

((mycompose
  (list (lambda (x) (+ 1 x))
        (lambda (x) (* 2 x))
        (lambda (x) (+ 3 x))))
 10)