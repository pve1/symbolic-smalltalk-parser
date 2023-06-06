(defpackage :symbolic-smalltalk-parser
  (:local-nicknames (:core :symbolic-smalltalk-core))
  (:use :cl))

(in-package :symbolic-smalltalk-parser)

(defclass terminal ()
  ((token :initarg :token :accessor token)))

(defclass non-terminal ()
  ((value :initarg :value :accessor value :initform nil)))

(defgeneric terminalp (thing)
  (:method (thing) nil))

(defun non-keyword-symbol-p (x)
  (and (symbolp x) (not (keywordp x))))

(defun non-keyword-symbol-string= (string x)
  (check-type string string)
  (and (non-keyword-symbol-p x)
       (string= string x)))

(defgeneric looking-at (something tokens)
  (:method (something tokens)
    (if (terminalp something)
        (looking-at-terminal something tokens)
        (looking-at-non-terminal something tokens)))
  (:method ((something symbol) tokens)
    (if (keywordp something)
        (eq something (first tokens))
        (call-next-method)))
  (:method ((something string) tokens)
    (if (non-keyword-symbol-string= something (first tokens))
        (values t (rest tokens))
        (values nil tokens)))
  (:method ((something null) tokens)
    (values t tokens)))

(defgeneric looking-at-terminal (terminal tokens)
  (:method :around (terminal tokens)
    (if tokens
        (if (call-next-method)
            (values t (rest tokens))
            (values nil tokens))
        (values nil nil))))

(defgeneric looking-at-non-terminal (non-terminal tokens)
  (:method (non-terminal tokens)
    (loop :for branch :in (non-terminal-branches non-terminal)
          :thereis (if (null branch)
                       t
                       (looking-at (first branch) tokens)))))

(defvar *consume-hook* nil)

(defun consume (something tokens)
  (consume-actual something tokens))

(defgeneric consume-actual (something tokens)
  (:method :after (something tokens)
    (when *consume-hook*
      (funcall *consume-hook* something tokens)))
  (:method (something tokens)
    (if (terminalp something)
        (consume-terminal something tokens)
        (consume-non-terminal something tokens)))
  (:method ((something symbol) tokens)
    (if (and (keywordp something)
             (eq (first tokens) something))
        (values (make-instance 'terminal :token (first tokens))
                (rest tokens))
        (call-next-method)))
  (:method ((something string) tokens)
    (if (non-keyword-symbol-string= something (first tokens))
        (values (make-instance 'terminal :token (first tokens))
                (rest tokens))
        (syntax-error something tokens))))

(defun consume-safely (something tokens)
  (let* ((consumed 0)
         (*consume-hook* (lambda (something tokens)
                           (declare (ignore something tokens))
                           (if (< consumed 100)
                               (incf consumed)
                               (error "Consume limit exceeded.")))))
    (consume something tokens)))

(defun consume-toplevel (something tokens)
  (multiple-value-bind (result rest)
      (consume something tokens)
    (when rest
      (syntax-error 'end-of-input rest))
    (values result nil)))

(defgeneric consume-terminal (terminal tokens)
  (:method :around (terminal tokens)
    (if (looking-at-terminal terminal tokens)
        (call-next-method)
        (syntax-error terminal tokens)))
  (:method (terminal tokens)
    (values (make-instance terminal :token (first tokens))
            (rest tokens))))

(defun non-terminal-branch-matches-p (branch tokens)
  (looking-at (first branch) tokens))

(defun consume-non-terminal-branch (branch tokens)
  (values (loop :for item :in branch
                :collect (multiple-value-bind (match rest)
                             (consume-actual item tokens)
                           (setf tokens rest)
                           match))
          tokens))

(defun consume-non-terminal (non-terminal tokens)
  (loop :for branch :in (non-terminal-branches non-terminal)
        :when (non-terminal-branch-matches-p branch tokens)
          :return (multiple-value-bind (match rest)
                      (consume-non-terminal-branch branch tokens)
                    (return-from consume-non-terminal
                      (values (make-instance non-terminal :value match)
                              rest))))
  (syntax-error non-terminal tokens))

(defun syntax-error (what tokens)
  (error "Syntax error looking at ~S. Expected ~A."
         tokens
         ;; Hack
         (if (and (or (stringp what)
                      (symbolp what))
                  (string= what ""))
             "||"
             what)))

(defmethod print-object ((object terminal) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (token object) stream)))

(defmethod print-object ((object non-terminal) stream)
  (print-unreadable-object (object stream :type t)
    (prin1 (value object) stream)))

(defmacro define-terminal (name (var) &body predicate-body)
  `(progn
     (defclass ,name (terminal) ())
     (defmethod terminalp ((terminal (eql ',name))) t)
     (defmethod looking-at-terminal ((terminal (eql ',name)) tokens)
       (funcall (lambda (,var)
                  ,@predicate-body)
                (first tokens)))))

(defmacro define-non-terminal (name &body alternatives)
  `(progn
     (defmethod non-terminal-branches ((name (eql ',name)))
       ',alternatives)
     (defclass ,name (non-terminal)
       ())))
