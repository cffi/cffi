;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; early-types.lisp --- Low-level foreign type operations.
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

;;;# Early Type Definitions
;;;
;;; This module contains basic operations on foreign types.  These
;;; definitions are in a separate file because they may be used in
;;; compiler macros defined later on.

(in-package #:cffi)

;;;# Foreign Type Classes

(defvar *foreign-types* (make-hash-table)
  "Hash table of all user-defined foreign types.")

(defclass foreign-type ()
  ((name :initform nil :initarg :name :accessor name)
   (alias :initform nil :initarg :alias :accessor alias)
   (size :initform 1 :initarg :size :accessor size))
  (:documentation "Contains information about a basic foreign type."))

(defmethod print-object ((type foreign-type) stream)
  "Print a FOREIGN-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~A" (name type))))

(defclass foreign-struct-type (foreign-type)
  ((slots :initform (make-hash-table) :initarg :slots :accessor slots))
  (:default-initargs :alias :pointer)
  (:documentation "Hash table of plists containing slot information."))

(defun find-foreign-type (name)
  "Return the foreign type instance for NAME or nil."
  (gethash name *foreign-types*))

(defun (setf find-foreign-type) (value name)
  "Set the foreign type object for NAME to VALUE."
  (setf (gethash name *foreign-types*) value))

;;;# Foreign Type Aliases

(defun notice-type-definition (name alias)
  "Install a type definition for NAME aliased to ALIAS."
  (setf (find-foreign-type name)
        (make-instance 'foreign-type :name name :alias alias
                       :size (foreign-type-size alias))))

(defun builtin-type-p (symbol)
  "Return true if SYMBOL is a built-in CFFI type."
  (keywordp symbol))

(defun canonicalize-foreign-type (type)
  "Convert TYPE to a built-in type by following aliases.
Signals an error if the type cannot be resolved."
  (loop for xtype = type then (alias (find-foreign-type xtype))
        until (builtin-type-p xtype)
        when (null xtype) do (error "Undefined foreign type> ~S." type)
        finally (return xtype)))

(defmacro defctype (name type)
  "Define NAME to be an alias for foreign type TYPE."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-type-definition ',name ',type)))

