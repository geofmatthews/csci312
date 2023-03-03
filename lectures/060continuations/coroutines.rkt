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

(define make-matcher-cor
  (lambda (tree-cor-1 tree-cor-2)
    (coroutine dummy-init-arg resume
      (let loop ()
        (let ((leaf1 (resume tree-cor-1 'get-a-leaf))
              (leaf2 (resume tree-cor-2 'get-a-leaf)))
          (if (eqv? leaf1 leaf2)
              (if (null? leaf1) #t (loop))
              #f))))))

(define make-leaf-gen-cor
  (lambda (tree matcher-cor)
    (coroutine dummy-init-arg
      (let loop ((tree tree))
        (cond ((null? tree) 'skip)
              ((pair? tree)
               (loop (car tree))
               (loop (cdr tree)))
              (else
               (resume matcher-cor tree))))
      (resume matcher-cor '()))))

(define same-fringe?
  (lambda (tree1 tree2)
    (letrec ((tree-cor-1
              (make-leaf-gen-cor
               tree1
               (lambda (v) (matcher-cor v))))
             (tree-cor-2
              (make-leaf-gen-cor
               tree2
               (lambda (v) (matcher-cor v))))
             (matcher-cor
              (make-matcher-cor
               (lambda (v) (tree-cor-1 v))
               (lambda (v) (tree-cor-2 v)))))
      (matcher-cor 'start-the-ball-rolling))))


(same-fringe? '(a (b c)) '((a b) c))
