(in-package :common-lisp-user)

(defpackage :cl-trivial-nntp
  (:nicknames :cl-trivial-nntp)
  (:use :cl :split-sequence)
  (:export
   #:run-nntp))

