(in-package :symbolic-smalltalk-parser)

(defun consumed-all (what tokens)
  (multiple-value-bind (match rest)
      (consume-safely what tokens)
    (and (typep match what)
         (null rest))))

(defmacro check-consumed (consume-safely-form &optional (remaining 0))
  `(ignore-errors
    (multiple-value-bind (match rest)
        ,consume-safely-form
      (and (typep match (second (second ',consume-safely-form)))
           (= (length rest) ,remaining)))))

(defmacro check-syntax-error (consume-safely-form)
  `(not (eq t (ignore-errors ,consume-safely-form t))))

(self-test.seed:define-self-test unary-message
  (check-consumed (consume-safely 'unary-message '(foo ·)) 1)
  (check-syntax-error (consume-safely 'unary-message '(+)))
  (check-syntax-error (consume-safely 'unary-message '()))
  (check-syntax-error (consume-safely 'unary-message '(:foo)))
  (check-consumed (consume-safely 'unary-message-chain '(foo bar ·)) 1)
  (check-consumed (consume-safely 'unary-message-chain '(1)) 1)
  (check-consumed (consume-safely 'unary-message-chain '()) 0))

(self-test.seed:define-self-test binary-message
  (check-consumed (consume-safely 'binary-message '(+ 1 foo ·)) 1)
  (check-syntax-error (consume-safely 'binary-message '(foo ·)))
  (check-consumed (consume-safely 'binary-message-chain '(+ 1 foo + 2 bar ·)) 1)
  (check-consumed (consume-safely 'binary-message-chain '(+ 1 + 2 ·)) 1)
  (check-consumed (consume-safely 'binary-message-chain '(1 foo)) 2)
  (check-consumed (consume-safely 'binary-message-chain '()) 0)
  (check-syntax-error (consume-safely 'binary-message-chain '(+)))
  (check-syntax-error (consume-safely 'binary-message-chain '(+ +))))

(self-test.seed:define-self-test keyword-message
  (check-consumed (consume-safely 'keyword-message-segment '(:foo 1 ·)) 1)
  (check-syntax-error (consume-safely 'keyword-message-segment '(1)))
  (check-consumed (consume-safely 'keyword-message '(:foo 1 :bar 2 ·)) 1)
  (check-consumed (consume-safely 'keyword-message '(:foo 1 foo ·)) 1)
  (check-consumed (consume-safely 'keyword-message '(:foo 1 foo :bar 2 ·)) 1)
  (check-syntax-error (consume-safely 'keyword-message-segment '(:foo)))
  (check-syntax-error (consume-safely 'keyword-message-segment '(:foo :bar))))

(self-test.seed:define-self-test message-chain
  (check-consumed (consume-safely 'message-chain '(:foo 1 foo :bar 2 foo bar ·)) 1)
  (check-consumed (consume-safely 'message-chain '(+ foo ·)) 1)
  (check-consumed (consume-safely 'message-chain '(foo ·)) 1)
  (check-syntax-error (consume-safely 'message-chain '(1)))
  (check-syntax-error (consume-safely 'message-chain '(+)))
  (check-syntax-error (consume-safely 'message-chain '())))

(self-test.seed:define-self-test expression
  (check-consumed (consume-safely 'expression '(1 :foo 1 foo :bar 2 foo bar ·)) 1)
  (check-consumed (consume-safely 'expression '(1 :foo 2 ··· :bar a ·)) 1)
  (check-consumed (consume-safely 'expression '(1 :foo 2 ··· :bar a ··· foo ·)) 1)
  (check-syntax-error (consume-safely 'expression '(:foo 1 ·)))
  (check-syntax-error (consume-safely 'expression '()))
  (check-syntax-error (consume-safely 'expression '(·))))

(self-test.seed:define-self-test statement
  (check-consumed (consume-safely 'statement '(a)))
  (check-consumed (consume-safely 'statement '(a ·)) 1)
  (check-consumed (consume-safely 'statement '(a foo)))
  (check-consumed (consume-safely 'statement '(a :foo foo bar)))
  (check-consumed (consume-safely 'statement '(a := b foo)))
  (check-consumed (consume-safely 'statement '(a := b := c foo))))

(self-test.seed:define-self-test statement-chain
  (check-consumed (consume-safely 'statement-chain '()))
  (check-consumed (consume-safely 'statement-chain '(a)))
  (check-consumed (consume-safely 'statement-chain '(a ·)))
  (check-consumed (consume-safely 'statement-chain '(a · b)))
  (check-consumed (consume-safely 'statement-chain '(a · b ·)))
  (check-consumed (consume-safely 'statement-chain '(a := foo · b ·)))
  (check-consumed (consume-safely 'statement-chain '(·)) 1))

(self-test.seed:define-self-test executable-code
  (check-consumed (consume-safely 'executable-code '(|| a ||)))
  (check-consumed (consume-safely 'executable-code '(|| a b c ||)))
  (check-consumed (consume-safely 'executable-code '(|| a || foo )))
  (check-consumed (consume-safely 'executable-code '(|| a || foo ·)))
  (check-consumed (consume-safely 'executable-code '(|| a || foo · bar)))
  (check-consumed (consume-safely 'executable-code '(|| a || foo · bar ·)))
  (check-consumed (consume-safely 'executable-code '(|| a || foo · ^ bar)))
  (check-consumed (consume-safely 'executable-code '(|| a || foo · ^ bar ·)))
  (check-consumed (consume-safely 'executable-code '(|| a || a := foo · ^ bar ·)))
  (check-consumed (consume-safely 'executable-code '(foo)))
  (check-consumed (consume-safely 'executable-code '(foo · bar)))
  (check-consumed (consume-safely 'executable-code '(foo · bar ·)))
  (check-consumed (consume-safely 'executable-code '(foo bar · foo bar ·)))
  (check-consumed (consume-safely 'executable-code '(a := foo bar · b := foo bar ·)))
  (check-consumed (consume-safely 'executable-code '(:= foo)) 2)
  (check-consumed (consume-safely 'executable-code '([ a ])))
  (check-consumed (consume-safely 'executable-code '([ a ] foo))))

(self-test.seed:define-self-test block-literal
  (check-consumed (consume-safely 'block-literal '([ ])))
  (check-consumed (consume-safely 'block-literal '([ 1 ])))
  (check-consumed (consume-safely 'block-literal '([ a foo · b bar ])))
  (check-consumed (consume-safely 'block-literal '([ a foo · b bar · ])))
  (check-consumed (consume-safely 'block-literal '([ :a || ])))
  (check-consumed (consume-safely 'block-literal '([ :a || a foo ])))
  (check-syntax-error (consume-safely 'block-literal '([ :a ])))
  (check-syntax-error (consume-safely 'block-literal '([ :a a foo ])))
  (check-syntax-error (consume-safely 'block-literal '([ || a foo ])))
  (check-syntax-error (consume-safely 'block-literal '([ || ])))
  (check-syntax-error (consume-safely 'block-literal (read-from-string "( [ )")))
  (check-syntax-error (consume-safely 'block-literal (read-from-string "( ] )"))))

(self-test.seed:define-self-test dynamic-array
  (check-consumed (consume-safely 'operand '( { } )))
  (check-consumed (consume-safely 'operand '( { foo } )))
  (check-consumed (consume-safely 'operand '( { foo · bar } )))
  (check-consumed (consume-safely 'operand '( { foo · bar · } )))
  (check-consumed (consume-safely 'operand '( { foo bar } ))))

(self-test.seed:define-self-test method-declaration
  (check-consumed (consume-safely 'method-declaration '(foo)))
  (check-consumed (consume-safely 'method-declaration '(foo ^ 1)))
  (check-consumed (consume-safely 'method-declaration '(foo || a ||)))
  (check-consumed (consume-safely 'method-declaration '(foo || a || ^ a)))
  (check-consumed (consume-safely 'method-declaration '(+ a a)))
  (check-consumed (consume-safely 'method-declaration '(+ a ^ a)))
  (check-consumed (consume-safely 'method-declaration '(+ a || b || ^ a)))
  (check-consumed (consume-safely 'method-declaration '(:foo a ^ a)))
  (check-consumed (consume-safely 'method-declaration '(:foo a :bar b ^ a)))
  (check-consumed (consume-safely 'method-declaration '(:foo a :bar b || c || ^ a)))
  (check-consumed (consume-safely 'method-declaration '(:foo a :bar b || c || c := 1 · ^ a)))
  (check-syntax-error (consume-safely 'method-declaration '(|| a ||)))
  (check-syntax-error (consume-safely 'method-declaration '(1)))
  (check-syntax-error (consume-safely 'method-declaration '("a")))
  (check-syntax-error (consume-safely 'method-declaration '( [ a ] )))
  (check-syntax-error (consume-safely 'method-declaration '(·))))
