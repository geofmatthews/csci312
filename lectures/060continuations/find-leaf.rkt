(define (find-leaf obj tree)
  (call/cc
   (lambda (cc)
     (letrec ((iter
	       (lambda (tree)
		 (cond
		  ((null?  tree) #f)
		  ((pair? tree)
		   (iter (car tree))
		   (iter (cdr tree)))
		  (else
		   (if (eqv? obj tree)
		       (cc obj)))))))
       (iter tree)))))

(find-leaf 7 '(1 (2 3) 4 (5 (6 7))))
(find-leaf 8 '(1 (2 3) 4 (5 (6 7))))
