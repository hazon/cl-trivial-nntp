(defpackage #:cl-trivial-nntp-asd
  (:use :cl :asdf))

(in-package :cl-trivial-nntp-asd)

(defsystem cl-trivial-nntp
    :name "cl-trivial-nntp"
    :author "Francesco Frigo"
    :version "0.1.0"
    :licence "GPLv2+"
    :description "Trivial NNTP client in Common Lisp"
    :components ((:file "package")
		 (:file "news" :depends-on ("package")))
    :depends-on (:split-sequence :usocket))

