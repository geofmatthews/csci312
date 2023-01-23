;   Bintree from EOPL p. 46 in Common Lisp
;   Illustrating CLOS (common lisp object system)
;   Geoffrey Matthews
;   Tested in CLISP
;   Thu Apr  4 10:26:22 PST 2002   

;;;; Note the separation of data and methods.
;;;; They can be declared together, object-style, or
;;;; separately, union-style.
;;;; In the following, print-tree is object-style, and
;;;; leaf-sum is union-style.

;;;; Leaf class:

(defclass leaf ()
  ((datum :accessor datum
	  :initarg :datum
	  :initform 0)))

(defmethod print-tree ((item leaf))
  (print-tree-indented item 0))

(defmethod print-tree-indented ((item leaf) depth)
  (dotimes (i depth) (format t "  "))
  (format t "~a~%" (datum item)))

;;;; Interior-node class:

(defclass interior-node ()
  ((key   :accessor key
	  :initarg :key
	  :initform "Dummy")
   (left  :accessor left
	  :initarg :left
	  :initform nil)
   (right :accessor right
	  :initarg :right
	  :initform nil)))

(defmethod print-tree ((item interior-node))
  (print-tree-indented item 0))

(defmethod print-tree-indented ((item interior-node) depth)
  (dotimes (i depth) (format t "  "))
  (format t "~a~%" (key item))
  (print-tree-indented (left item) (+ depth 1))
  (print-tree-indented (right item) (+ depth 1)))
  

;;;; Leaf-sum method:

(defmethod leaf-sum ((item leaf))
  (datum item))

(defmethod leaf-sum ((item interior-node))
  (+ (leaf-sum (left item))
     (leaf-sum (right item))))

(setq x
      (make-instance
       'interior-node
       :key "Fuzzy"
       :left (make-instance
	      'interior-node
	      :key "Wuzzy"
	      :left (make-instance 'leaf :datum 11)
	      :right (make-instance
		      'interior-node
		      :key "Was"
		      :left (make-instance 'leaf :datum 22)
		      :right (make-instance 'leaf :datum 33)))
       :right (make-instance
	       'interior-node
	       :key "A"
	       :left (make-instance
		      'interior-node
		      :key "Bear"
		      :left (make-instance 'leaf :datum 44)
		      :right (make-instance 'leaf :datum 55))
	       :right (make-instance 'leaf :datum 66))))

(format t "~%")
(print-tree x)
(format t "The sum is: ~a~%" (leaf-sum x))
      



