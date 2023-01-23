#lang racket

(define (tree-producer tree k)
  (let/cc k
    (if (not (cons? tree))
        (k tree)
        (tree-producer
         (car tree
              (lambda (result)
                (tree-pro

(define tree1 (cons 'a (cons 'b 'c)))
(define tree2 (cons (cons 'a 'b) 'c))

(define t1 (tree-producer tree1))


           