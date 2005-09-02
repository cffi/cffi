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

;;;# Foreign Types

(defvar *foreign-types* (make-hash-table)
  "Hash table of all user-defined foreign types.")

(defun find-type (name)
  "Return the foreign type instance for NAME or nil."
  (gethash name *foreign-types*))
  
(defun find-type-or-lose (name)
  "Return the foreign type instance for NAME or signal an error."
  (or (find-type name)
      (error "Undefined foreign type: ~S" name)))

(defun notice-foreign-type (type)
  "Inserts TYPE in the *FOREIGN-TYPES* hashtable."
  (setf (gethash (name type) *foreign-types*) type)
  (name type))

;;;# Parsing Type Specifications
;;;
;;; Type specifications are of the form (type {args}*). The
;;; type parser can specify how its arguments should look like
;;; through a lambda list.
;;;
;;; "type" is a shortcut for "(type)", ie, no args were specified.
;;;
;;; Examples of such types: boolean, (boolean), (boolean :int)
;;; If the boolean type parser specifies the lambda list:
;;; &optional (base-type :int), then all of the above three
;;; type specs would be parsed to an identical type.
;;;
;;; Type parsers, defined with DEFINE-TYPE-SPEC-PARSER should
;;; return a subtype of the foreign-type class.

(defvar *type-parsers* (make-hash-table)
  "Hash table of defined type parsers.")

(defun find-type-parser (symbol)
  "Return the type parser for SYMBOL."
  (gethash symbol *type-parsers*))

(defun (setf find-type-parser) (func symbol)
  "Set the type parser for SYMBOL."
  (setf (gethash symbol *type-parsers*) func))

(defmacro define-type-spec-parser (symbol lambda-list &body body)
  "Define a type parser on SYMBOL and lists whose CAR is SYMBOL."
  (when (stringp (car body)) ; discard-docstring
    (setq body (cdr body)))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (find-type-parser ',symbol)
           (lambda ,lambda-list ,@body))))

(defun parse-type (type-spec-or-name)
  (or (find-type type-spec-or-name)
      (let* ((type-spec (mklist type-spec-or-name))
             (parser (find-type-parser (car type-spec))))
        (if parser
            (let ((new-type (apply parser (cdr type-spec))))
              (notice-foreign-type new-type)
              new-type)
            (error "Unknown CFFI type: ~S." type-spec-or-name)))))

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

;;; Ever since I changed translators to get the type as an argument,
;;; naming anonymous types is not really important. TODO: decide if
;;; this is still desirable nevertheless. --luis

(defvar *anon-count* 0
  "Counter for anonymous types.")

(defclass foreign-type ()
  ((name
    ;; Name of this foreign type, a symbol.
    ;:initform (error "A type name is required.")
    :initarg :name
    :accessor name)
   ;; The following slots form the basis for the type
   ;; translator mechanism. (implemented in types.lisp)
   (to-c-converter   :initform nil)
   (to-c-expander    :initform nil)
   (from-c-converter :initform nil)
   (from-c-expander  :initform nil)
   (to-c-dynamic-expander :initform nil))
  (:documentation "Contains information about a basic foreign type."))

(defmethod initialize-instance :after ((self foreign-type) &key)
  "Give a unique name to FOREIGN-TYPE in case none was specified."
  (unless (slot-boundp self 'name)
    (setf (name self)
          (intern (format nil "~A-~A-~D"
                          (symbol-name '#:anon)
                          (class-name (class-of self))
                          (post-incf *anon-count*))))))

(defmethod print-object ((type foreign-type) stream)
  "Print a FOREIGN-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (name type))))

(defun canonicalize-foreign-type (type)
  "Convert TYPE to a built-in type by following aliases.
Signals an error if the type cannot be resolved."
  (canonicalize (parse-type type)))

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
  "Defines a built-in foreign-type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-type
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
    :accessor size)
   (alignment
    ;; This struct's alignment requirements
    :initarg :alignment
    :accessor alignment))
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

(defmethod foreign-type-alignment ((type foreign-struct-type))
  "Return the alignment requirements for this struct."
  (alignment type))