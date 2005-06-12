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

;;;# Generic Functions on Types

(defgeneric canonicalize (foreign-type)
  (:documentation
   "Return the built-in foreign type for FOREIGN-TYPE.
Signals an error if FOREIGN-TYPE is undefined."))

(defgeneric aggregatep (foreign-type)
  (:documentation
   "Return true if FOREIGN-TYPE is an aggregate type."))

(defgeneric foreign-type-alignment (foreign-type)
  (:documentation
   "Return the structure alignment in bytes of a foreign type."))

(defgeneric foreign-type-size (foreign-type)
  (:documentation
   "Return the size in bytes of a foreign type."))

;;;# Foreign Types

(defclass foreign-type ()
  ((name
    ;; Name of this foreign type, a symbol.
    :initform (error "A type name is required.")
    :initarg :name
    :accessor name))
  (:documentation "Contains information about a basic foreign type."))

(defmethod print-object ((type foreign-type) stream)
  "Print a FOREIGN-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (name type))))

(defvar *foreign-types* (make-hash-table)
  "Hash table of all user-defined foreign types.")

(defun find-foreign-type (name)
  "Return the foreign type instance for NAME or nil."
  (or (gethash name *foreign-types*)
      (error "Undefined foreign type: ~S" name)))

(defun (setf find-foreign-type) (value name)
  "Set the foreign type object for NAME to VALUE."
  (setf (gethash name *foreign-types*) value))

(defun canonicalize-foreign-type (type)
  "Convert TYPE to a built-in type by following aliases.
Signals an error if the type cannot be resolved."
  (canonicalize (find-foreign-type type)))

;;;# Built-In Foreign Types

(defclass foreign-built-in-type (foreign-type)
  ((type-keyword
    ;; Keyword in CFFI-SYS representing this type.
    :initform (error "A type keyword is required.")
    :initarg :type-keyword
    :accessor type-keyword))
  (:documentation "A built-in foreign type."))

(defmethod canonicalize ((type foreign-built-in-type))
  "Return the built-in type keyword for TYPE."
  (type-keyword type))

(defmethod aggregatep ((type foreign-built-in-type))
  "Returns false, built-in types are never aggregate types."
  nil)

(defmethod foreign-type-alignment ((type foreign-built-in-type))
  "Return the alignment of a built-in type."
  (%foreign-type-alignment (type-keyword type)))

(defmethod foreign-type-size ((type foreign-built-in-type))
  "Return the size of a built-in type."
  (%foreign-type-size (type-keyword type)))

(defmacro define-built-in-foreign-type (keyword)
  "Defines a built-in foreign type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (find-foreign-type ,keyword)
           (make-instance 'foreign-built-in-type :name ,keyword
                          :type-keyword ,keyword))))

;;;# Foreign Typedefs

(defclass foreign-typedef (foreign-type)
  ((actual-type
    ;; The FOREIGN-TYPE instance this type is an alias for.
    :initarg :actual-type
    :accessor actual-type))
  (:documentation "A type that aliases another type."))

(defmethod canonicalize ((type foreign-typedef))
  "Return the built-in type keyword for TYPE."
  (canonicalize (actual-type type)))

(defmethod aggregatep ((type foreign-typedef))
  "Return true if TYPE's actual type is aggregate."
  (aggregatep (actual-type type)))

(defmethod foreign-type-alignment ((type foreign-typedef))
  "Return the alignment of a foreign typedef."
  (foreign-type-alignment (actual-type type)))

(defmethod foreign-type-size ((type foreign-typedef))
  "Return the size in bytes of a foreign typedef."
  (foreign-type-size (actual-type type)))

(defun notice-type-definition (name alias)
  "Install a type definition for NAME aliased to ALIAS."
  (setf (find-foreign-type name)
        (make-instance 'foreign-typedef :name name
                       :actual-type (find-foreign-type alias))))

(defmacro defctype (name type)
  "Define NAME to be an alias for foreign type TYPE."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-type-definition ',name ',type)))

;;;# Structure Type

(defclass foreign-struct-type (foreign-type)
  ((slots
    ;; Hash table of slots in this structure, keyed by name.
    :initform (make-hash-table)
    :initarg :slots
    :accessor slots)
   (size
    ;; Cached size in bytes of this structure.
    :initarg :size
    :accessor size))
  (:documentation "Hash table of plists containing slot information."))

(defmethod canonicalize ((type foreign-struct-type))
  "Returns :POINTER, since structures can not be passed by value."
  :pointer)

(defmethod aggregatep ((type foreign-struct-type))
  "Returns true, structure types are aggregate."
  t)

(defmethod foreign-type-size ((type foreign-struct-type))
  "Return the size in bytes of a foreign structure type."
  (size type))
