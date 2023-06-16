(in-package :symbolic-smalltalk-parser)

(defgeneric emit (object &rest arguments)
  (:method ((object terminal) &rest arguments)
    (declare (ignore arguments))
    (token object))
  (:method ((object nested-expression) &rest arguments)
    (declare (ignore arguments))
    (with-accessors ((token token)) object
      (unless (or (typep token 'non-terminal)
                  (typep token 'terminal))
        (setf token (consume 'statement token)))
      (emit token)))
  (:method ((object non-terminal) &rest arguments)
    (declare (ignore arguments))
    (let ((val (value object)))
      (if (= 1 (length val))
          (emit val)
          (error "Don't know how to emit ~S." object)))))

(defun value-matches-pattern-p (object pattern)
  (let ((value (value object)))
    (and (= (length value)
            (length pattern))
         (loop :for part :in pattern
               :for child :in value
               :always (typep child part)))))

(defun list-with-car-p (car list &key (test #'eql))
  (and (listp list)
       (funcall test (car list) car)))

(defmacro define-emit (object-class arguments &body body)
  (alexandria:with-gensyms (args ?n ?args)
    `(defmethod emit ((,object-class ,object-class) &rest ,args)
       (destructuring-bind ,arguments ,args
         (macrolet (($ (,?n)
                      `(nth ,,?n (value ,',object-class)))
                    (? (,?n &rest ,?args)
                      `(emit ($ ,,?n) ,@,?args)))
           (cond ,@(loop :for (pattern . pattern-body) :in body
                         :collect (if (eq pattern t)
                                      `(t ,@pattern-body)
                                      `((value-matches-pattern-p ,object-class ',pattern)
                                        nil
                                        ,@pattern-body)))))))))

;;; (? N) means "call emit on the Nth argument of the value branch".

(define-emit operand ()
  (t (? 0)))

(define-emit dynamic-array-literal ()
  ((terminal statement-chain terminal)
   `(vector ,@(? 1))))

(define-emit block-literal ()
  ((terminal block-contents terminal)
   (? 1)))

(define-emit block-contents ()
  ((formal-block-argument-declaration terminal block-executable-code)
   `(lambda ,(? 0 (? 1))
      ,(? 2)))
  ((block-executable-code)
   `(lambda ()
      ,(? 0))))

;; Like executable-code, except no named block.
(define-emit block-executable-code ()
  ((local-variable-declaration-list statement-chain)
   `(let ,(? 0)
      ,@(? 1)))
  ((statement-chain)
   `(progn ,@(? 0)))
  (()))

(define-emit formal-block-argument-declaration (terminal)
  ((valid-keyword formal-block-argument-declaration-chain)
   `(,(intern (string (? 0)) (symbol-package terminal))
     ,@(? 1 terminal))))

(define-emit formal-block-argument-declaration-chain (terminal)
  ((valid-keyword formal-block-argument-declaration-chain)
   `(,(intern (string (? 0)) (symbol-package terminal))
     ,@(? 1 terminal)))
  (()))

(define-emit unary-message (operand)
  ((identifier)
   `(core:send ,operand ,(? 0))))

(define-emit unary-message-chain (operand)
  ((unary-message unary-message-chain)
   (? 1 (? 0 operand)))
  (() operand))

(define-emit binary-message-operand ()
  ((operand unary-message-chain)
   (? 1 (? 0))))

(define-emit binary-message (operand)
  ((binary-operator binary-message-operand)
   `(core:send ,operand ,(? 0) ,(? 1))))

(define-emit binary-message-chain (operand)
  ((binary-message binary-message-chain)
   (? 1 (? 0 operand)))
  (() operand))

(define-emit keyword-message-argument ()
  ((binary-message-operand binary-message-chain)
   (? 1 (? 0))))

(define-emit keyword-message-segment ()
  ((valid-keyword keyword-message-argument)
   `(,(? 0) ,(? 1))))

(define-emit keyword-message (operand)
  ((keyword-message-segment keyword-message-chain)
   (? 1 operand (? 0))))

(define-emit keyword-message-chain (operand acc)
  ((keyword-message-segment keyword-message-chain)
   (? 1 operand `(,@acc ,@(? 0))))
  (() (if acc
          `(core:send ,operand ,@acc)
          operand)))

(define-emit message-chain (operand)
  ((unary-message unary-message-chain binary-message-chain keyword-message-chain)
   (? 3 (? 2 (? 1 (? 0 operand))) nil))
  ((binary-message binary-message-chain keyword-message-chain)
   (? 2 (? 1 (? 0 operand)) nil))
  ((keyword-message)
   (? 0 operand)))

(define-emit expression-message (operand)
  ((message-chain cascaded-message-chain)
   (? 1 (? 0 operand) nil))
  (() operand))

(define-emit cascaded-message-chain (operand acc)
  ((cascade-operand message-chain cascaded-message-chain)
   (? 2 operand `(,@acc ,(? 1 'dummy))))
  (() (if acc
          `(core:cascade* ,operand
             ,@acc)
          operand)))

(define-emit expression ()
  ((operand expression-message)
   (? 1 (? 0))))

(define-emit statement-expression (operand)
  ((terminal operand statement-expression) ; Assignment
   (check-type operand symbol)
   `(setf ,operand ,(? 2 (? 1))))
  ((expression-message)
   (? 0 operand)))

(define-emit statement ()
  ((operand statement-expression)
   (? 1 (? 0))))

(define-emit statement-chain ()
  ((statement between-statements)
   `(,(? 0) ,@(? 1)))
  ((final-statement)
   `(,(? 0)))
  (()))

(define-emit between-statements ()
  ((statement-separator statement-chain)
   (? 1))
  (()))

(define-emit final-statement ()
  ((return-operator statement optional-statement-separator)
   `(return-from executable-code ,(? 1))))

(define-emit optional-statement-separator ()
  ((statement-separator))
  ())

(define-emit local-variable-declaration-list ()
  ((terminal identifier-list terminal)
   (? 1)))

(define-emit identifier-list ()
  ((identifier identifier-list)
   `(,(? 0) ,@(? 1)))
  (()))

(define-emit executable-code ()
  ((local-variable-declaration-list statement-chain)
   `(block executable-code
      (let ,(? 0)
        ,@(? 1)
        ,(core:self))))
  ((statement-chain)
   (let* ((statements (? 0))
          (have-return-from?
            (list-with-car-p 'return-from
                             (alexandria:last-elt statements))))

     `(block executable-code
        ,@statements
        ;; Return self by default.
        ,@(if have-return-from?
              nil
              (list (core:self))))))
  (()))

(define-emit method-header ()
  (t (? 0)))

(define-emit unary-method-header ()
  ((unary-message)
   (cddr (? 0 'dummy))))

(define-emit binary-method-header ()
  ((binary-operator identifier)
   `(,(? 0) ,(? 1))))

(define-emit keyword-method-header-segment ()
  ((valid-keyword identifier)
   `(,(? 0) ,(? 1))))

(define-emit keyword-method-header-segment-chain ()
  ((keyword-method-header-segment keyword-method-header-segment-chain)
   `(,@(? 0) ,@(? 1)))
  (()))

(define-emit keyword-method-header ()
  ((keyword-method-header-segment keyword-method-header-segment-chain)
   `(,@(? 0) ,@(? 1))))

(define-emit method-declaration ()
  ((method-header executable-code)
   (alexandria:with-gensyms (class)
     `(lambda (,class)
        (core:add-selector-to-class ,class
                                    ',(? 0)
                                    '(,(? 1)))))))
