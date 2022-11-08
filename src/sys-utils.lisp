;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; sys-utils.lisp --- Various utilities.
;;;
;;; Copyright (C) 2022, Stelian Ionescu <sionescu@cddr.org>
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

(in-package #:cffi-sys)

(defun quoted-form-p (form)
  (and (proper-list-p form)
       (= 2 (length form))
       (eql 'quote (car form))))

(defun constant-form-p (form &optional env)
  (let ((form (if (symbolp form)
                  (macroexpand form env)
                  form)))
    (or (quoted-form-p form)
        (constantp form env))))

(defun constant-form-value (form &optional env)
  (declare (ignorable env))
  (cond
    ((quoted-form-p form)
     (second form))
    (t
     #+clozure
     (ccl::eval-constant form)
     #+sbcl
     (sb-int:constant-form-value form env)
     #-(or clozure sbcl)
     (eval form))))
