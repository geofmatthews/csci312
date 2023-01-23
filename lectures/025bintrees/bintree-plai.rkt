#lang plai
;   Bintree 
;   Illustrating define-type as variant structures
;   Geoffrey Matthews
;   Wed Mar 30 08:33:43 PDT 2011


(define-type bintree
  (leaf
   (datum number?))
  (interior-node
   (key string?)
   (left bintree?)
   (right bintree?)))

(define (print-tree tree . depth)
    (if (null? depth) (print-tree tree 0)
        (type-case bintree tree
          (leaf (datum)
                (begin
            (let loop ((i 0))
              (when (< i (car depth)) (begin (display "  ") (loop (+ i 1)))))
            (display datum) 
            (newline)))
          (interior-node (key left right)
                         (begin
            (let loop ((i 0))
              (when (< i (car depth)) (begin (display "  ") (loop (+ i 1)))))
            (display key)
            (newline)
            (print-tree left (+ (car depth) 1))
            (print-tree right (+ (car depth) 1)))))))
           
(define (leaf-sum tree)
    (type-case bintree tree
           (leaf (datum) datum)
           (interior-node (key left right)
                          (+ (leaf-sum left) (leaf-sum right)))))

(define x 
  (interior-node
   "Fuzzy"
   (interior-node 
    "Wuzzy"
    (leaf 11)
    (interior-node
     "Was"
     (leaf 22)
     (leaf 33)))
   (interior-node
    "A"
    (interior-node
     "Bear"
     (leaf 44)
     (leaf 55))
    (leaf 66))))

(print-tree x)
(display "The sum is: ")
(display (leaf-sum x))
(newline)

