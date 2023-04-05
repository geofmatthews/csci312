#lang plai

(define-type BFAE
  [num (n number?)]
  [add (l BFAE?) (r BFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body BFAE?)]
  [app (fun-expr BFAE?) (arg-expr BFAE?)]
  [bool (tf boolean?)]
  [&& (l BFAE?) (r BFAE?)]
  [OR (l BFAE?) (r BFAE?)]
  [!! (tf BFAE?)]
  [<< (n1 BFAE?) (n2 BFAE?)]
  [=== (n1 BFAE?) (n2 BFAE?)]
  [myif (expr BFAE?) (then BFAE?) (else BFAE?)])

(define-type BFAE-Value
  [numV (n number?)]
  [closureV (param symbol?)
            (body BFAE?)
            (env Environment?)]
  [boolV (bool boolean?)])

(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value BFAE-Value?) (env Environment?)])

#|
Parser
|#
(define (parse sexp)
  (cond
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    [(boolean? sexp) (bool sexp)]
    [(list? sexp)
     (case (first sexp)
       [(+) (add (parse (second sexp))
                 (parse (third sexp)))]
       [(fun) (fun (first (second sexp)) (parse (third sexp)))]
       [(and) (&& (parse (second sexp)) (parse (third sexp)))]
       [(or) (OR (parse (second sexp)) (parse (third sexp)))]
       [(not) (!! (parse (second sexp)))]
       [(<) (<< (parse (second sexp)) (parse (third sexp)))]
       [(=) (=== (parse (second sexp)) (parse (third sexp)))]
       [(if) (myif (parse (second sexp))
                   (parse (third sexp))
                   (parse (fourth sexp)))]
       [else (app (parse (first sexp))
                  (parse (second sexp)))]
       )
     ]
    )
  )

#|
Lookup function
|#
(define (lookup name env)
  (type-case Environment env
    [mtSub () (error 'lookup "no binding for identifier")]
    [aSub (bound-name bound-value rest-env)
          (if (symbol=? bound-name name)
              bound-value
              (lookup name rest-env))]))

#|
Add function
|#
(define (num+ n1 n2)
  (numV (+ (numV-n n1) (numV-n n2))))

#|
Conditional functions- and, or, not, <, and =
|#
(define (bool&& l r)
  (boolV (and (boolV-bool l) (boolV-bool r))))

(define (boolOR l r)
  (boolV (or (boolV-bool l) (boolV-bool r))))

(define (bool!! x)
  (boolV (not (boolV-bool x))))

(define (num<< n1 n2)
  (boolV (< (numV-n n1) (numV-n n2))))

(define (num== n1 n2)
  (boolV (= (numV-n n1) (numV-n n2))))

#|
Interpreter
|#
(define (interp expr ds)
  (type-case BFAE expr
    [num (n) (numV n)]
    [add (l r) (num+ (interp l ds) (interp r ds))]
    [id (v) (lookup v ds)]
    [fun (bound-id bound-body)
         (closureV bound-id bound-body ds)]
    [app (fun-expr arg-expr)
         (local ([define fun-val (interp fun-expr ds)])
           (interp (closureV-body fun-val)
                   (aSub (closureV-param fun-val)
                         (interp arg-expr ds)
                         (closureV-env fun-val))))]
    [bool (x) (boolV x)]
    [&& (l r) (bool&& (interp l ds) (interp r ds))]
    [OR (l r) (boolOR (interp l ds) (interp r ds))]
    [!! (x) (bool!! (interp x ds))]
    [<< (n1 n2) (num<< (interp n1 ds) (interp n2 ds))]
    [=== (n1 n2) (num== (interp n1 ds) (interp n2 ds))]
    [myif (expr then else) (if (boolV-bool (interp expr ds))
                                 (interp then ds)
                                 (interp else ds))]))

(test
 (interp (parse '(and (or #t #f) #t))
              (mtSub))
 (boolV #t))

(test
 (interp
  (parse
   '(if (= 0 1) 2 3))
  (mtSub))
 (numV 3))

(test
 (interp
  (parse
   '((fun (f) (f (and #t #f)))
     (fun (x) (if x 10 20))))
  (mtSub))
 (numV 20)
)