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

(defclass foreign-enum (foreign-typedef)
  ((keyword-values
    :initform (make-hash-table :test 'eq)
    :reader keyword-values)
   (value-keywords
    :initform (make-hash-table)
    :reader value-keywords))
  (:documentation "Describes a foreign enumerated type."))

(defun make-foreign-enum (type-name base-type values)
  "Makes a new instance of the foreign-enum class."
  (let ((type (if (eq type-name :enum)
                  (make-instance 'foreign-enum
                                 :actual-type (parse-type base-type))
                  (make-instance 'foreign-enum :name type-name
                                 :actual-type (parse-type base-type))))
        (default-value 0))
    (dolist (pair values)
      (destructuring-bind (keyword &optional (value default-value))
          (mklist pair) 
        (check-type keyword keyword)
        (check-type value integer)
        (if (gethash keyword (keyword-values type))
            (error "A foreign enum cannot contain duplicate keywords: ~S."
                   keyword)
            (setf (gethash keyword (keyword-values type)) value))
        (if (gethash value (value-keywords type))
            (error "A foreign enum cannot contain duplicate values: ~S."
                   value)
            (setf (gethash value (value-keywords type)) keyword))
        (setq default-value (1+ value))))
    type))

;;; These [four] functions could be good canditates for compiler macros
;;; when the value or keyword is constant.  I am not going to bother
;;; until someone has a serious performance need to do so though.
(defun %foreign-enum-value (type keyword)
  (check-type keyword keyword)
  (or (gethash keyword (keyword-values type))
      (error "~S is not defined as a keyword for enym type ~S."
               keyword type)))

(defun foreign-enum-value (type keyword)
  (let ((type-obj (parse-type type)))
    (if (not (typep type-obj 'foreign-enum))
      (error "~S is not a foreign enum type." type)
      (%foreign-enum-value type-obj keyword))))

(defun %foreign-enum-keyword (type value)
  (check-type value integer)
  (or (gethash value (value-keywords type))
      (error "~S is not defined as a value for enum type ~S."
             value type)))

(defun foreign-enum-keyword (type value)
  (let ((type-obj (parse-type type)))
    (if (not (typep type-obj 'foreign-enum))
        (error "~S is not a foreign enum type." type)
        (%foreign-enum-keyword type-obj value))))

(defmacro defcenum (name &body enum-list)
  "Define an foreign enumerated type."
  (discard-docstring enum-list)
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (notice-foreign-type
        (make-foreign-enum ',name :int ',enum-list)))
     ;; to-c translator
     (define-type-translator ,name :to-c (type value)
       `(if (keywordp ,value)
            (%foreign-enum-value ,type ,value)
            ,value))
     ;; from-c translator
     (define-type-translator ,name :from-c (type value)
       `(%foreign-enum-keyword ,type ,value))
     ',name))

;;;# Anonymous ENUM type.

(define-type-spec-parser :enum (&rest values)
  "Parser for anonymous enum types."
  (let ((type-obj (make-foreign-enum :enum :int values)))
    (notice-foreign-type type-obj)
    ;; to-c translator
    (install-type-translator
     type-obj
     (lambda (type-obj type value)
       (declare (ignore type-obj))
       `(if (keywordp ,value)
            (%foreign-enum-value ,type ,value)
            ,value))
     (lambda (type value)
       (if (keywordp value)
           (%foreign-enum-value type value)
           value))
     :to-c)
    ;; from-c translator
    (install-type-translator
     type-obj
     (lambda (type-obj type value)
       (declare (ignore type-obj))
       `(%foreign-enum-keyword ,type ,value))
     (lambda (type value) (%foreign-enum-keyword type value))
     :from-c)
    type-obj))
