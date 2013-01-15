;;;; A trivial NNTP client written in Common Lisp.
;;;;
;;;; Copyright (C) 2012, Francesco Frigo
;;;;
;;;; This program is free software; you can redistribute it and/or modify
;;;; it under the terms of the GNU General Public License as published by
;;;; the Free Software Foundation; either version 2 of the License, or
;;;; (at your option) any later version.

(in-package :cl-trivial-nntp)

(defun read-single-line (socket)
  (remove-if #'(lambda (x) (eq x #\Return)) 
	     (read-line (usocket:socket-stream socket) nil)))

(defun send-only-command (socket command)
  ;;(format t ">> sending [~a]~%" command)
  (format (usocket:socket-stream socket) "~a~a~a" command #\Return #\Newline)   
  (force-output (usocket:socket-stream socket)))

(defun send-short-command (socket command)
  (send-only-command socket command)
  (split-sequence #\Space (read-single-line socket)))

(defun send-long-command (socket command)
  (values (send-short-command socket command)
	  (loop for line = (read-single-line socket)
	     while (not (equal line ".")) collect line)))

(defun get-article-list (socket group)
  (send-long-command socket (format nil "listgroup ~a" group)))

(defun get-article-headers (socket article-id)
  (send-long-command socket (format nil "head ~a" article-id)))

(defun get-article-body (socket article-id)
  (send-long-command socket (format nil "body ~a" article-id)))

;; ======================================================================

(defun save-article (socket group article-id file-spec)
  (when group
    (send-short-command socket (format nil "group ~a" group)))
  (multiple-value-bind (res data) (get-article-body socket article-id)
    (declare (ignorable res))
    (with-open-file (stream file-spec :direction :output 
			    :if-exists :error :if-does-not-exist :create)
      (dolist (d data)
	(format stream "~a~%" d)))))

(defun save-group (socket group dir-spec)
  (send-short-command socket (format nil "group ~a" group))
  (multiple-value-bind (res lst) (get-article-list socket group)
    (declare (ignorable res))
    (dolist (id lst)
      ;;(format t "Processing: ~a~%" id)
      (save-article socket nil id (merge-pathnames id dir-spec)))))

;; ======================================================================

(defun print-article-list (socket group)
  (format t "Articles in group: ~a~%" group)
  (multiple-value-bind (res lst) (get-article-list socket group)
    (declare (ignorable res))
    (format t "R: ~a~%" res)
    (format t "~a~%" lst)))

(defun print-article-headers (socket article-id)
  (format t "Headers for article ID: ~a~%" article-id)
  (multiple-value-bind (res hdr) (get-article-headers socket article-id)
    (declare (ignorable res))
    (format t "R: ~a~%" res)
    (dolist (h hdr)
      (format t "~a~%" h))))

(defun print-article-body (socket article-id)
  (format t "Body for article ID: ~a~%" article-id)
  (multiple-value-bind (res data) (get-article-body socket article-id)
    (declare (ignorable res))
    (format t "R: ~a~%" res)
    (dolist (d data)
      (format t "~a~%" d))))

;; ======================================================================

(defun run-nntp (host port)
  (let ((socket (usocket:socket-connect host port)))
    (format t "~a~%" (read-single-line socket))
    (save-group socket "alt.os.development" #P"/tmp/news/")
    (send-short-command socket "quit")
    (usocket:socket-close socket)))

;; (run-nntp "freenews.netfront.net" 119)
;; (run-nntp "news.synserver.de" 119)

