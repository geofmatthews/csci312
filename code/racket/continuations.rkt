#lang racket
;;; Examples of using Scheme continuations
;;; jhbrown@alum.mit.edu

;;; The first thing you do when using call-with-current-continuation
(define call/cc call-with-current-continuation)

;;; ------------------------------------------------------------
;;; Early exit using continuations
(define evencount 0)

(let ((test 17))
  (call/cc (lambda (return)
             (when (odd? test) (return 5))
             (set! evencount (+ evencount 1))
             7)))
;;; ==> 5

;;; ------------------------------------------------------------
;;; Multiple-value continuations for multiple-value return
(call-with-values
    (lambda () (values 4 5))   ; producer
  (lambda (a b) (+ a b)))      ; continuation


;;; ------------------------------------------------------------
;;; Simple exceptions using continuations (see SRFI-34 for a more
;;; complete exception-system)

(define top-exception-handler  (lambda () (error "unhandled exception")))
(define (throw) (top-exception-handler))

(define-syntax
  try
  (syntax-rules ()
    ((try catch-clause body ...)
     (let* ((result #f)
	    (old-handler top-exception-handler)
	    (success (call/cc (lambda (cont)
				(set! top-exception-handler (lambda () (cont #f)))
				(display 'foo) 
				(set! result (begin body ...))
				#t))))
       (set! top-exception-handler old-handler)
       (if success result (catch-clause))))))

;;; ------------------------------------------------------------
;;; backtracking using continuations
;;; Adapted from ``Teach yourself Scheme in Fixnum Days (TYSiFD)'', by Dorai Sitaram

(define amb-fail '())
(define (initialize-amb-fail)
  (set! amb-fail (lambda (x) (error "amb tree exhausted"))))

(define-syntax amb
  (syntax-rules ()
    ((amb argument ...)
     (let ((old-amb-fail amb-fail))
       (call/cc (lambda (return)
		  (call/cc (lambda (next)
			     (set! amb-fail next)
			     (return argument))) ...
		  (set! amb-fail old-amb-fail)
		  (amb-fail #f)))))))


(define-syntax bag-of
  (syntax-rules ()
    ((bag-of expr)
     (let* ((old-amb-fail amb-fail)
	    (result '()))
       (when (call/cc (lambda (ifcondcont)
		      (set! amb-fail ifcondcont)
		      (let ((e expr))
			(set! result (cons e result))
			(ifcondcont #t))))
	   (amb-fail #f))
       (set! amb-fail old-amb-fail)
       result))))
		       

(define (assert pred)
  (when (not pred) (amb)))

;;; demo of basic ambiguity operator 
(initialize-amb-fail)
(let ((value (amb 0 1 2 3 4 5 6)))
  (assert (> value 2))
  (assert (even? value))
  value)

(amb)
;;; (amb) -- if we call it any more, we'll fail when we load this file.

;;; demo of nested ambs, and bag-of
(define (three-dice sumto)
  (let ((die1 (amb 1 2 3 4 5 6))
	(die2 (amb 1 2 3 4 5 6))
	(die3 (amb 1 2 3 4 5 6)))
    (assert (= sumto (+ die1 die2 die3)))
    (list die1 die2 die3)))

(initialize-amb-fail)
(bag-of (three-dice 4))
(bag-of
 (let ((sum (amb 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18)))
   (bag-of (three-dice sum))))


;;; Another method of using amb...
(define (amb-list rest)
  (let ((old-amb-fail amb-fail))
    (call/cc (lambda (return)
	       (for-each (lambda (value)
			   (call/cc (lambda (next)
				      (set! amb-fail next)
				      (return value))))
			 rest)
	       (set! amb-fail old-amb-fail)
	       (amb-fail #f)))))

(define (range low high)
  (if (> low high) '()
      (cons low (range (+ 1 low) high))))

(define (numbers-that-divide argument)
  (let* ((number (amb-list (range 1 argument))))
    (assert (zero? (modulo argument number)))
    number))

(bag-of
 (let* ((frob (amb-list (range 1 10)))
	(bar (amb-list (range 1 10))))
					;  (display (cons frob bar))
   (assert (> frob 9))
   frob))



;;; ------------------------------------------------------------
;;; Iterators

;;; code for simple list traversal
(define (list-traverse list)
  (when (pair? list)
      (list-traverse list)))

;;; code for list iterator (no continuations)
(define (list-iter list)
  (lambda ()
    (if (pair? list)
	(let ((value (car list)))
	  (set! list (cdr list))
	  value)
	'())))

;;; example iterator
(define li (list-iter '(1 2 3 4 5)))
(li)
(li)
(li)
(li)
(li)
(li)


;;;------------------------------------------------------------
;;; Tree iterator

;;; Tree traversal: nice and simple
(define (tree-traverse tree)
  (when (pair? tree)
      (begin
	(tree-traverse (car tree))
	(tree-traverse (cdr tree)))))

;;; Tree iterator, written to explicitly maintain stack state (no
;;; continuations) Kind of ugly.
(define (tree-iter-painful tree)
  (let ((cell-stack (list tree)))
    (lambda ()
      (display cell-stack)
      (display "\n")
      (if cell-stack
	  (let loop ((node (pop! cell-stack)))
	    (if (pair? node)
		(begin
		  (push! (cdr node) cell-stack)
		  (loop (car node)))
		node))
	  '()))))

;;; send macro, used in tree-iter with continuations, below
(define-syntax send 
  (syntax-rules ()
    ((send to from value)
     (call/cc
      (lambda (state)
	(set! from (lambda () (state 0)))
	(to value))))))

;;; with-caller macro.  I don't like the name or syntax; suggestions?
;;; used in tree-iter with continuations, below
(define-syntax with-caller
  (syntax-rules ()
    ((with-caller caller localstate body ...) 
     (let ((caller #f))
       (letrec ((localstate
		 (lambda ()
		   body ...)))
	 (lambda ()
	   (call/cc
	    (lambda (caller-cont)
	      (set! caller caller-cont)
	      (localstate)))))))))

;;; Tree traversal with continuations (and macros, defined below).
;;; Note close structural resemblance to the traversal routine, above.
(define (tree-iter tree)
  (with-caller caller loopstate
   (let loop ((node tree))
     (if (pair? node)
	 (begin
	   (loop (car node))
	   (loop (cdr node)))
	 (begin
	   (send caller loopstate node)
	   '())))))


;;; demo...
(define ti (tree-iter '((1 . 2) . (3 . 4))))
(ti)
(ti)
(ti)
(ti)
(ti)


;;; ------------------------------------------------------------
;;; simple, cooperating multithreading primitives
;;; Note: using a queue instead of a stack would make
;;; yield behave more fairly.

;;; simple stack operations
(define-syntax push!
  (syntax-rules ()
    ((push! value stack)
     (set! stack (cons value stack)))))

(define-syntax pop!
  (syntax-rules ()
    ((pop! stack)
     (let ((top (car stack)))
       (set! stack (cdr stack))
       top))))

(define (empty-stack? stack)
  (not (pair? stack)))

;;; global variables used by the threading scheme
(define thread-set '())
(define scheduler-context #f)

;;; Spawn new thread (breadth-first style)
(define (spawn thunk)
  (push! (lambda () (thunk) (scheduler-context 0))
	 thread-set))

;;; Let another runnable thread run
(define (yield)
  (call/cc
   (lambda (this-thread)
     (when (not (empty-stack? thread-set))
	 (let ((next-thread (pop! thread-set)))
	   (push! (lambda () (this-thread 0)) thread-set)
	   (next-thread))))))

;;; Start multithreading, using this thunk.  This is necessary because
;;; the one thing we don't want to do while "multithreading" is return
;;; control to the REPL until all threads are done.
(define (start-scheduling thunk)
  (set! thread-set '())
  (call/cc
   (lambda (scheduler)
     (set! scheduler-context scheduler)
     (spawn thunk)))
  (when (not (empty-stack? thread-set))
      (begin
	 ((pop! thread-set))
;	 (loop)
	 (display "\n\n**Scheduler exiting**.\n\n"))))


;;; Trivial example using two threads
#|
(start-scheduling
 (lambda ()
   (spawn (lambda ()
	    (display "sub-thread\n")
	    (yield)
	    (display "more sub-thread\n")
	    (yield)))
   (display "first thread\n")
   (yield)
   (display "and more first\n")))
|#


  

