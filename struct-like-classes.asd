; -*- mode: lisp -*-

(asdf:defsystem :struct-like-classes
  :description "Macros for classes defined with defstruct-like shorthand."
  :version "0.1"
  :license "MIT"
  :author "Mike Watters <mike@mwatters.net>"
  :depends-on (:alexandria
               :iterate)
  :components ((:file "struct-like-classes")))
