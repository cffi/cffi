;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; objects.lisp --- Foreign typed objects.
;;;
;;; Copyright (C) 2005, Luis Oliveira  <loliveira(@)common-lisp.net>
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

(in-package #:cffi)

#||
(defstruct (foreign-object (:conc-name fobject-)
                           (:print-object print-foreign-object))
  pointer type (valid-p t))

(defun print-foreign-object (fobject stream)
  "Print a FOREIGN-OBJECT to STREAM unreadably."
  (print-unreadable-object (fobject stream :type t :identity t)
    (format stream "of type ~S" (name (fobject-type fobject)))))

;; initial-element initial-contents
(defun make-fobject (&key type address)
  (let ((the-type (parse-type)))
    (make-foreign-object
     :pointer (or address (foreign-alloc (foreign-type-size the-type)))
     :type the-type)))

(defun fobject-free (fobject)
  (if (fobject-valid-p fobject)
      (progn (setf (fobject-valid-p fobject) nil)
             (foreign-free (fobject-pointer fobject)))
      (warn "Trying to free an invalid foreign object.")))

(defun copy-fobject (fobject)
  )

(defun fobject-eq (fobject1 fobject2)
  )

(defun coerce-fobject (fobject new-type)
  )

(defmacro with-coerced-fobject ((var) fobject)
  )

(defun deref ()
  )
||#
;;;# Boolean Type

(define-foreign-type :boolean (&optional (base-type :int))
  "Boolean type. Maps to an :int by default. Only accepts integer types."
  (ecase base-type
    ((:char
      :unsigned-char
      :int
      :unsigned-int
      :long
      :unsigned-long) base-type)))

(define-type-translator :boolean :from-c (type value)
  `(not (zerop ,value)))

(define-type-translator :boolean :to-c (type value)
  `(if ,value 1 0))