;;;; This file contains the system definition for ASDF

(in-package :asdf-user)

(defsystem #:ltrace
  :description "My own take at a lisp process hacking"
  :version "2.1.0"
  :author "liamm91"
  :licence "MIT"
  :depends-on ("cffi" "cl-ppcre")
  :serial t
  :components
  ((:file "src/package")
   (:file "src/utils")
   (:file "src/memmappings")))
