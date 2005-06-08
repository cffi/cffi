;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enum.lisp --- Defining foreign constants as Lisp keywords.
;;;
;;; Copyright (C) 2005, James Bielman  <jamesjb@jamesjb.com>
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

;;;# Foreign Constants as Lisp Keywords
;;;
;;; This module defines the DEFCENUM macro, which provides an
;;; interface for defining a type and associating a set of integer
;;; constants with keyword symbols for that type.
;;;
;;; The keywords are automatically translated to the appropriate
;;; constant for the type by a type translator when passed as
;;; arguments or a return value to a foreign function.

(defclass foreign-enum (foreign-type)
  ((keyword-values
    :initform (make-hash-table)
    :reader keyword-values)
   (value-keywords
    :initform (make-hash-table :test 'equal)
    :reader value-keywords))
  (:documentation "Describes a foreign enumerated type."))

(defun notice-foreign-enum (type-name base-type values)
  "Defines TYPE-NAME to be a foreign enum type."
  (let ((type (make-instance 'foreign-enum :name type-name
                             :alias base-type
                             :size (foreign-type-size base-type))))
    (loop for (keyword value) in values
          do (setf (gethash keyword (keyword-values type)) value)
             (setf (gethash value (value-keywords type)) keyword))
    (setf (find-foreign-type type-name) type)))

;;; These two functions could be good canditates for compiler macros
;;; when the value or keyword is constant.  I am not going to bother
;;; until someone has a serious performance need to do so though.
(defun foreign-enum-value (type keyword)
  "Return the numeric value of KEYWORD as enum TYPE."
  (check-type type symbol)
  (check-type keyword keyword)
  (let ((type-obj (find-foreign-type type)))
    (when (or (null type-obj) (not (typep type-obj 'foreign-enum)))
      (error "~S is not a foreign enum type." type))
    (or (gethash keyword (keyword-values type-obj))
        (error "~S is not defined as a keyword for enum type ~S."
               keyword type))))

(defun foreign-enum-keyword (type value)
  "Return the keyword value of a numer VALUE as enum TYPE."
  (check-type type symbol)
  (check-type value integer)
  (let ((type-obj (find-foreign-type type)))
    (when (or (null type-obj) (not (typep type-obj 'foreign-enum)))
      (error "~S is not a foreign enum type." type))
    (or (gethash value (value-keywords type-obj))
        (error "~S is not defined as a value for enum type ~S."
               value type))))

(defmacro defcenum (name &body values)
  "Define an foreign enumerated type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-enum ',name :int ',values)
     (define-type-translator ,name :in (arg result-var)
       (declare (ignore result-var))
       (values `(if (keywordp ,arg)
                    (foreign-enum-value ',',name ,arg)
                    ,arg)
               nil))
     (define-type-translator ,name :result (arg result-var)
       (declare (ignore result-var))
       (values `(foreign-enum-keyword ',',name ,arg) nil))))
