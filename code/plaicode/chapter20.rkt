(require plai)
(define-type KCFAE
  [num (n number?)]
  [add (left KCFAE?) (right KCFAE?)]
  [id (name symbol?)]
  [fun (param symbol?) (body KCFAE?)]
  [app (fun-expr KCFAE?) (arg-expr KCFAE?)]
  [if0 (condition KCFAE?) (then KCFAE?) (else KCFAE?)]
  [bindcc (var symbol?) (body KCFAE?)])

(define-type Environment
  [mtSub]
  [aSub (name symbol?) (value KCFAE-Value?) (env Environment?)])

;; lookup : symbol Environment -> FAE-Value
(define (lookup name env)
  (type-case Environment env
             [mtSub () (error 'lookup "no binding for identifier")]
             [aSub (bound-name bound-value rest-env)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-env))]))

;; parse : sexpr -> KCFAE

(define (parse sexpr)
  (cond [(number? sexpr) (num sexpr)]
        [(symbol? sexpr) (id sexpr)]
        [(list? sexpr)
         (case (first sexpr)
           [(+) (add (parse (second sexpr))
                     (parse (third sexpr)))]
           [(fun) (fun (first (second sexpr))
                       (parse (third sexpr)))]
           [(if0) (if0 (parse (second sexpr))
                       (parse (third sexpr))
                       (parse (fourth sexpr)))]
           [(bindcc) (bindcc (second sexpr) 
                             (parse (third sexpr)))]
           [else (app (parse (first sexpr))
                      (parse (second sexpr)))]
           )]))

(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)]
  [contV (c procedure?)])

(define (num+ faev1 faev2)
  (type-case KCFAE-Value faev1
             [numV (n) (numV (+ n
                                (type-case KCFAE-Value faev2
                                           [numV (m) m]
                                           [else (error 'add-args "Adding non-numbers")])))]
             [else (error 'add-args "Adding non-numbers")]))

(define (num-zero? faev)
  (type-case KCFAE-Value faev
             [numV (n) (zero? n)]
             [else #f]))
;; interp : KCFAE Env receiver -> doesn’t return
(define (interp expr env k)
  (type-case KCFAE expr
             [num (n) (k (numV n))]
             [add (l r) (interp l env
                                (lambda (lv)
                                  (interp r env
                                          (lambda (rv)
                                            (k (num+ lv rv))))))]
             [if0 (test truth falsity)
                  (interp test env
                          (lambda (tv)
                            (if (num-zero? tv)
                                (interp truth env k)
                                (interp falsity env k))))]
             [id (v) (k (lookup v env))]
             [fun (param body)
                  (k (closureV (lambda (arg-val dyn-k)
                                 (interp body (aSub param arg-val env) dyn-k))))]
             [app (fun-expr arg-expr)
                  (interp fun-expr env
                          (lambda (fun-val)
                            (interp arg-expr env
                                    (lambda (arg-val)
                                      (type-case KCFAE-Value fun-val
                                                 [closureV (c) (c arg-val k)]
                                                 [contV (c) (c arg-val)]
                                                 [else (error "not an applicable value")])))))]
             [bindcc (cont-var body)
                     (interp body
                             (aSub cont-var
                                   (contV k)
                                   env)
                             k)]))

(define (top-k x)
  (begin
    (display "Top level.")
    (newline)
    x))

(test (interp (parse '(bindcc k 3))
              (mtSub)
              top-k)
      (numV 3))

(test (interp (parse '(bindcc k (k 3)))
              (mtSub)
              top-k)
      (numV 3))

(test (interp (parse '(bindcc k (+ 1 (k 3))))
              (mtSub)
              top-k)
      (numV 3))
(test (interp (parse '(+ 1 (bindcc k (+ 1 (k 3)))))
              (mtSub)
              top-k)
      (numV 4))
(test
 (interp (parse '((bindcc k
                          (k (fun (dummy) 3)))
                  1729))
         (mtSub)
         top-k)
 (numV 3))
(test
 (interp (parse '(bindcc k
                         (k
                          (k
                           (k 3)))))
         (mtSub)
         top-k)
 (numV 3))
(test
 (interp (parse '(((bindcc k k)
                   (fun (x) x))
                  3))
         (mtSub)
         top-k)
 (numV 3))
(test
 (interp (parse '((((bindcc k k)
                    (fun (x) x))
                   (fun (x) x))
                  3))
         (mtSub)
         top-k)
 (numV 3))