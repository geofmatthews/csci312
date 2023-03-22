(define (show x) (display x) (newline))

(define (for init condition change body result)
  (local [(define (loop init result)
            (if (condition init)
                (loop (change init)
                      (body init result))
                result))]
    (loop init result)))

(for 10 positive? sub1 + 0)

(define (mycopy ls)
  (cond ((empty? ls) '())
        (else (cons (car ls) (mycopy (cdr ls))))))
(show 'mycopy)
(mycopy '(1 2 3 4 5))
(define (myfoldr op init ls)
  (cond ((empty? ls) '())
        (else
         (op (car ls) (myfoldr op init (cdr ls))))))
(show 'myfoldr)
(myfoldr cons '() '(1 2 3 4))

(define (revcopy ls init)
  (cond ((empty? ls) init)
        (else (revcopy (cdr ls) (cons (car ls) init)))))
(show 'revcopy)
(revcopy '(1 2 3 4 5) '())

(define (myfoldl op init ls)
  (cond ((empty? ls) init)
        (else
         (myfoldl op (op (car ls) init) (cdr ls)))))

(define (sum ls)
  (foldl + 0 ls))
(show 'sum)
(sum '(1 2 3 4 5))

(define (myreverse ls)
  (foldl cons '() ls))
(show 'reverse)
(myreverse '(1 2 3 4 5))

(define (mycopy ls)
  (foldr cons '() ls))
(show 'copy)
(mycopy '(1 2 3 4 5))

(define (myappend ls1 ls2)
  (foldr cons ls2 ls1))
(show 'append)
(myappend '(1 2 3) '(4 5 6))

(define (myall pred ls)
  (foldr (lambda (x y) (and (pred x) y)) #\t ls))
(show 'all)
(myall even? '(2 4 6))
(myall even? '(2 4 7))

(define (mymap func ls)
  (foldr (lambda (x y) (cons (func x) y)) '() ls))
(show 'map)
(mymap (lambda (x) (* 2 x)) '(1 2 3 4))

(define (myfilter pred ls)
  (foldr (lambda (x y) (if (pred x) (cons x y) y)) '() ls))
(show 'filter)
(filter even? '(1 2 3 4 5 6 7 8 9))

(define (mypartition pred ls)
  (foldr (lambda (x y) (if (pred x)
                           (list (cons x (car y)) (cadr y))
                           (list (car y) (cons x (cadr y)))))
         (list '() '())
         ls))
(show 'partition)
(mypartition even? '(1 2 3 4 5 6 7 8 9))

(define (myunique ls)
  (foldr (lambda (x y) (if (member x y) y (cons x y))) '() ls))
(show 'unique)
(myunique '(1 2 2 3 4 5 4 4 3 3 2 1 7 9 7))


(define (spread x ls)
  (cond ((empty? ls) '())
        (else (cons (cons x (car ls)) (spread x (cdr ls))))))
(show 'spread)
(spread 99 '((1 2 3) (4 5 6) () (7)))

(define (powerset ls)
  (cond ((empty? ls) '(()))
        (else
         (let ((ps (powerset (cdr ls))))
           (append ps
                   (spread (car ls) ps))))))
(show 'powerset)
(powerset '(1 2 3))

(define (extend-powerset x ps)
  (foldl (lambda (item rest)
           (cons item
                 (cons
                  (cons x item)
                  rest)))
         '()
         ps))
(show 'extend-powerset)
(extend-powerset 3 '(() (1) (2) (1 2)))

(define (powersetfold ls)
  (foldl extend-powerset
         '(())
         ls))
  
(show 'powersetfold)
(powersetfold '(1 2 3))
         