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
            (apply parser (cdr type-spec))
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

(defclass foreign-type ()
  ((name
    ;; Name of this foreign type, a symbol.
    :initform (gensym "ANONYMOUS-CFFI-TYPE")
    :initarg :name
    :accessor name))
  (:documentation "Contains information about a basic foreign type."))

(defmethod print-object ((type foreign-type) stream)
  "Print a FOREIGN-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (name type))))

(defmethod make-load-form ((type foreign-type) &optional env)
  "Return the form used to dump types to a FASL file."
  (declare (ignore env))
  `(find-type-or-lose ',(name type)))

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
;;;
;;; We have two classes: foreign-type-alias and foreign-typedef.
;;; The former is a direct super-class of the latter. The only
;;; difference between the two is that foreign-typedef has different
;;; behaviour wrt type translations. (see types.lisp)

(defclass foreign-type-alias (foreign-type)
  ((actual-type
    ;; The FOREIGN-TYPE instance this type is an alias for.
    :initarg :actual-type
    :accessor actual-type))
  (:documentation "A type that aliases another type."))

(defmethod canonicalize ((type foreign-type-alias))
  "Return the built-in type keyword for TYPE."
  (canonicalize (actual-type type)))

(defmethod aggregatep ((type foreign-type-alias))
  "Return true if TYPE's actual type is aggregate."
  (aggregatep (actual-type type)))

(defmethod foreign-type-alignment ((type foreign-type-alias))
  "Return the alignment of a foreign typedef."
  (foreign-type-alignment (actual-type type)))

(defmethod foreign-type-size ((type foreign-type-alias))
  "Return the size in bytes of a foreign typedef."
  (foreign-type-size (actual-type type)))

(defclass foreign-typedef (foreign-type-alias)
  ())

;;; This should probably be an argument to parse-type.
;;; So we'd have: (parse-type foo :follow-typedefs t)
;;; instead of (follow-typedefs (parse-type foo)) ? --luis
(defun follow-typedefs (type)
  (if (eq (type-of type) 'foreign-typedef)
      (follow-typedefs (actual-type type))
      type))

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
