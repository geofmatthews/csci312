#lang plai

(define (my-length ls)
  (if (empty? ls)
      0
      (+ 1 (my-length (cdr ls)))))

(test (my-length '(1 2 3)) 3)

(define (my-append ls1 ls2)
  (if (empty? ls1)
      ls2
      (cons (car ls1)
            (my-append (cdr ls1) ls2))))

(test (my-append '(1 2) '(3 4)) '(1 2 3 4))

(define (square-all ls)
  (if (empty? ls)
      '()
      (cons (expt (car ls) 2)
            (square-all (cdr ls)))))

(test (square-all '(1 2 3)) '(1 4 9))

(define (add-one-all ls)
  (if (empty? ls)
      '()
      (cons (add1 (car ls))
            (add-one-all (cdr ls)))))

(test (add-one-all '(1 2 3)) '(2 3 4))

(define (my-map f ls)
  (if (empty? ls)
      '()
      (cons (f (car ls))
            (my-map f (cdr ls)))))

(test (my-map add1 '(1 2 3)) '(2 3 4))
(test (my-map (lambda (x) (* x x)) '(1 2 3))
      '(1 4 9))

(define (evens ls)
  (cond ((empty? ls) '())
        ((even? (car ls))
         (cons (car ls) (evens (cdr ls))))
        (else
         (evens (cdr ls)))))

(test (evens '(1 2 3 4 5 6)) '(2 4 6))

(define (my-filter pred? ls)
  (cond ((empty? ls) '())
        ((pred? (car ls))
         (cons (car ls)
               (my-filter pred? (cdr ls))))
        (else (my-filter pred? (cdr ls)))))

(test (my-filter even? '(1 2 3 4 5 6))
      '(2 4 6))

(define (copy ls)
  (if (empty? ls)
      '()
      (cons (car ls) (cdr ls))))

(test (copy '(1 2 3)) '(1 2 3))

(define (reverse ls)
  (if (empty? ls)
      '()
      (my-append (reverse (cdr ls))
                 (list (car ls)))))

(test (reverse '(1 2 3)) '(3 2 1))

(define (power-set ls)
  (local
    [(define (distribute item ls)
       (if (empty? ls)
           '()
           (cons (cons item (car ls))
                 (distribute item (cdr ls)))))
     ]
    (if (empty? ls)
        (list '())
        (let ((subsets (power-set (cdr ls))))
          (append
           subsets
           (distribute (car ls)
                       (power-set (cdr ls))))))))

(let ((ls '(1 2 3 4)))
  (let ((len (my-length ls))
        (ps (power-set ls)))
    (print ps)
    (newline)
    (test (my-length ps)
          (expt 2 len))))

(define (my-sort ls less?)
  (local
    [(define (insert item ls)
       (cond ((empty? ls) (list item))
             ((less? item (car ls))
              (cons item ls))
             (else
              (cons (car ls)
                    (insert item (cdr ls))))))]
    (if (empty? ls)
        '()
        (insert (car ls)
                (my-sort (cdr ls) less?)))))

(test (my-sort '(3 2 1) <) '(1 2 3))

(define (my-less? set1 set2)
  (cond ((empty? set1) #t)
        ((empty? set2) #f)
        ((< (car set1) (car set2)) #t)
        ((> (car set1) (car set2)) #f)
        (else (my-less? (cdr set1) (cdr set2)))))

(test (my-sort (power-set '(1 2 3)) my-less?)
      (my-sort '(() (1) (2) (3)
           (1 2) (1 3) (2 3)
           (1 2 3)) my-less?))
        
