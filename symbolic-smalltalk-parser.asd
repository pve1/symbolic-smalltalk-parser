(asdf:defsystem :symbolic-smalltalk-parser
  :description "Provides the Symbolic-smalltalk parser."
  :author "Peter von Etter"
  :license "LGPL-3.0"
  :version "0.0.1"
  :serial t
  :components ((:file "parser")
               (:file "grammar"))
  :depends-on (#:symbolic-smalltalk-core
               #:alexandria))
