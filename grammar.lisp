(in-package :symbolic-smalltalk-parser)

;;; Inspired by http://chronos-st.blogspot.com/2007/12/smalltalk-in-one-page.html

;;; Terminals

(defun binary-operator-p (x)
  (and (symbolp x)
       (not (keywordp x))
       (loop :for c :across (symbol-name x)
             :always (loop :for op :across "-,~!@%&*+/=<>?|"
                             :thereis (eql c op)))))

(defun special-symbol-p (x)
  (and (symbolp x)
       (or (eq x :=)
           (member x '("·" "···" "^" "" "[" "]") :test #'string=))))

(define-terminal identifier (x)
  (and (symbolp x)
       (not (keywordp x))
       (not (special-symbol-p x))
       (not (binary-operator-p x))))

(define-terminal valid-keyword (x)
  (and (keywordp x)
       (not (special-symbol-p x))))

(define-terminal statement-separator (x)
  (symbol-and-string= "·" x))

(define-terminal cascade-operand (x)
  (symbol-and-string= "···" x))

(define-terminal return-operator (x)
  (symbol-and-string= "^" x))

(define-terminal assignment-operator (x)
  (eq x :=))

(define-terminal binary-operator (x)
  (binary-operator-p x))

(define-terminal literal (x)
  (typecase x
    (string t)
    (number t)
    (character t)
    (array t)
    (structure-object t)
    (cons (or (eq 'quote (car x))
              (eq 'function (car x))))))

;;; Non-terminals

(define-non-terminal block-literal
  ("[" block-contents "]"))

(define-non-terminal block-contents
  (formal-block-argument-declaration "" executable-code)
  (executable-code))

(define-non-terminal formal-block-argument-declaration
  (valid-keyword formal-block-argument-declaration-chain))

(define-non-terminal formal-block-argument-declaration-chain
  (valid-keyword formal-block-argument-declaration-chain)
  ())

(define-non-terminal operand
  (literal)
  (block-literal)
  (identifier))

(define-non-terminal unary-message
  (identifier))

(define-non-terminal unary-message-chain
  (unary-message unary-message-chain)
  ())

(define-non-terminal binary-message-operand
  (operand unary-message-chain))

(define-non-terminal binary-message
  (binary-operator binary-message-operand))

(define-non-terminal binary-message-chain
  (binary-message binary-message-chain)
  ())

(define-non-terminal keyword-message-argument
  (binary-message-operand binary-message-chain))

(define-non-terminal keyword-message-segment
  (valid-keyword keyword-message-argument))

(define-non-terminal keyword-message-chain
  (keyword-message-segment keyword-message-chain)
  ())

(define-non-terminal keyword-message
  (keyword-message-segment keyword-message-chain))

(define-non-terminal message-chain
  (unary-message unary-message-chain binary-message-chain keyword-message-chain)
  (binary-message binary-message-chain keyword-message-chain)
  (keyword-message))

(define-non-terminal cascaded-message
  (cascade-operand message-chain))

(define-non-terminal cascaded-message-chain
  (cascade-operand message-chain cascaded-message-chain)
  ())

(define-non-terminal expression-message
  (message-chain cascaded-message-chain)
  ())

(define-non-terminal expression
  (operand expression-message))

(define-non-terminal statement-expression
  (:= operand statement-expression)
  (expression-message))

(define-non-terminal statement
  (operand statement-expression))

(define-non-terminal statement-chain
  (statement between-statements)
  (final-statement)
  ())

(define-non-terminal between-statements
  (statement-separator statement-chain)
  ())

(define-non-terminal final-statement
  (return-operator statement optional-statement-separator))

(define-non-terminal optional-statement-separator
  (statement-separator)
  ())

(define-non-terminal local-variable-declaration-list
  ("" identifier-list ""))

(define-non-terminal identifier-list
  (identifier identifier-list)
  ())

(define-non-terminal executable-code
  (local-variable-declaration-list
   statement-chain)
  (statement-chain)
  ())
