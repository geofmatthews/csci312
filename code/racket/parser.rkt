#lang plai

;; Adaptation of calc.rkt to use plai datatypes
;; Geoffrey Matthews
;; 2011

(define-type WAE
  [num (n number?)]
  [add (lhs WAE?) (rhs WAE?)]
  [sub (lhs WAE?) (rhs WAE?)]
  [with (name symbol?) (named-expr WAE?) (body WAE?)]
  [id (name symbol?)])

;; An interactive calculator inspired by the calculator example in the bison manual.

;; Import the parser and lexer generators.
(require parser-tools/yacc
         parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

(define-tokens value-tokens (NUM VAR ))
(define-empty-tokens op-tokens (newline = OP CP + - EOF WITH ENDWITH))

;; A hash table to store variable values in for the calculator
(define vars (make-hash))

(define-lex-abbrevs
  (lower-letter (:/ "a" "z"))
  
  (upper-letter (:/ #\A #\Z))
  
  ;; (:/ 0 9) would not work because the lexer does not understand numbers.  (:/ #\0 #\9) is ok too.
  (digit (:/ "0" "9")))

(define wael
  (lexer
   [(eof) 'EOF]
   ;; recursively call the lexer on the remaining input after a tab or space.  Returning the
   ;; result of that operation.  This effectively skips all whitespace.
   [(:or #\tab #\space #\newline) (wael input-port)]
   ;; Since (token-=) returns '=, just return the symbol directly
   [(:or "+" "-") (string->symbol lexeme)]
   ["with" 'WITH]
   ["endwith" 'ENDWITH]
   ["(" 'OP]
   [")" 'CP]
   [(:+ (:or lower-letter upper-letter)) (token-VAR (string->symbol lexeme))]
   [(:+ digit) (token-NUM (string->number lexeme))]
   [(:: (:+ digit) #\. (:* digit)) (token-NUM (string->number lexeme))]))


(define waep
  (parser
   
   (start start)
   (end  EOF)
   (tokens value-tokens op-tokens)
   (error (lambda (a b c) (void)))
   
   (precs (left - +)
          )
   
   (grammar
    
    (start [() #f]
           ;; If there is an error, ignore everything before the error
           ;; and try to start over right after the error
           [(error start) $2]
           [(exp) $1])
    
    (exp [(NUM) (num $1)]
         [(VAR) (id $1)]
         [(exp + exp) (add $1 $3)]
         [(exp - exp) (sub $1 $3)]
         [(WITH OP VAR exp CP exp ENDWITH) (with $3 $4 $6)]
         [(OP exp CP) $2]
         ))))

(let ((ip (open-input-string
           "99 +  
               with (x 5)
                 (22 + x) - (5 + 2)
               endwith
           - 3")))
  (waep (lambda () (wael ip))))
