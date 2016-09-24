;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; grovel.lisp --- The CFFI Groveller.
;;;
;;; Copyright (C) 2005-2006, Dan Knap <dankna@accela.net>
;;; Copyright (C) 2005-2006, Emily Backes <lucca@accela.net>
;;; Copyright (C) 2007, Stelian Ionescu <sionescu@cddr.org>
;;; Copyright (C) 2007, Luis Oliveira <loliveira@common-lisp.net>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.
;;;

(in-package #:cffi-grovel)

(defun dirty-featurep (x)
  "Ugly in implementation but will always match the implementations logic"
  (with-standard-io-syntax
    (with-input-from-string (s (format nil "#+~s t" x))
      (read s nil nil))))

(defun make-caching-reader-conditional ()
  (let ((cache (make-array 0 :adjustable t :fill-pointer 0)))
    (list (lambda (stream sub-char numarg)
	    (declare (ignore numarg))
	    (let* ((feature-expr (let ((*package* (find-package :keyword))
				       (*read-suppress* nil))
				   (read stream t nil t)))
		   (present (dirty-featurep feature-expr))
		   (match (char= sub-char (if present #\+ #\-))))
	      (vector-push-extend feature-expr cache)
	      (if match
		  (read stream t nil t)
		  (let ((*read-suppress* t))
		    (read stream t nil t)
		    (values)))))
	  cache)))

(defun flatten-features (cache)
  (let ((feature-expressions (concatenate 'list cache))
        (ignored '(:or :and or and nil t))
        (features nil))
    (labels ((make-pairs (x) (list x (dirty-featurep x)))
             (feature? (x) (and (keywordp x) (not (member x ignored))))
             (dummy-walk (x)
               (when (feature? x) (push x features))
               nil))
      (subst-if nil #'dummy-walk feature-expressions)
      (mapcar #'make-pairs
              (sort (remove-duplicates (remove-if-not #'keywordp features))
                    #'string<)))))

(defun call-with-cached-reader-conditionals (func &rest args)
  (destructuring-bind (rfunc cache) (make-caching-reader-conditional)
    (let ((*readtable* (copy-readtable)))
      (set-dispatch-macro-character #\# #\+ rfunc *readtable*)
      (set-dispatch-macro-character #\# #\- rfunc *readtable*)
      (values (apply func args) (flatten-features cache)))))

(defmacro with-cached-reader-conditionals (&body body)
  `(call-with-cached-reader-conditionals (lambda () ,@body)))
