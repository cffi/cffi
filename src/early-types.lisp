;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; early-types.lisp --- Low-level foreign type operations.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
;;; Copyright (C) 2005-2007, Luis Oliveira  <loliveira@common-lisp.net>
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
;;;
;;; Type specifications are of the form (type {args}*). The type
;;; parser can specify how its arguments should look like through a
;;; lambda list.
;;;
;;; "type" is a shortcut for "(type)", ie, no args were specified.
;;;
;;; Examples of such types: boolean, (boolean), (boolean :int) If the
;;; boolean type parser specifies the lambda list: &optional
;;; (base-type :int), then all of the above three type specs would be
;;; parsed to an identical type.
;;;
;;; Type parsers, defined with DEFINE-PARSE-METHOD should return a
;;; subtype of the foreign-type class.

(defvar *type-parsers* (make-hash-table)
  "Hash table of defined type parsers.")

(defun find-type-parser (symbol)
  "Return the type parser for SYMBOL."
  (or (gethash symbol *type-parsers*)
      (error "Unknown CFFI type: ~S." symbol)))

(defun (setf find-type-parser) (func symbol)
  "Set the type parser for SYMBOL."
  (setf (gethash symbol *type-parsers*) func))

;;; Using a generic function would have been nicer but generates lots
;;; of style warnings in SBCL.  (Silly reason, yes.)
(defmacro define-parse-method (name lambda-list &body body)
  "Define a type parser on NAME and lists whose CAR is NAME."
  (discard-docstring body)
  (warn-if-kw-or-belongs-to-cl name)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (find-type-parser ',name)
           (lambda ,lambda-list ,@body))
     ',name))

;;; Utility function for the simple case where the type takes no
;;; arguments.
(defun notice-foreign-type (name type)
  (setf (find-type-parser name) (lambda () type))
  name)

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

(defgeneric unparse-type (foreign-type)
  (:documentation
   "Unparse FOREIGN-TYPE to a type specification (symbol or list)."))

;;;# Foreign Types

(defclass foreign-type ()
  ()
  (:documentation "Base class for all foreign types."))

(defmethod make-load-form ((type foreign-type) &optional env)
  "Return the form used to dump types to a FASL file."
  (declare (ignore env))
  `(parse-type ',(unparse-type type)))

(defmethod foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (foreign-type-size (parse-type type)))

(defclass named-foreign-type (foreign-type)
  ((name
    ;; Name of this foreign type, a symbol.
    :initform (error "Must specify a NAME.")
    :initarg :name
    :accessor name)))

(defmethod print-object ((type named-foreign-type) stream)
  "Print a FOREIGN-TYPEDEF instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (name type))))

;;; Return the type's name which can be passed to PARSE-TYPE.  If
;;; that's not the case for some subclass of NAMED-FOREIGN-TYPE then
;;; it should specialize UNPARSE-TYPE.
(defmethod unparse-type ((type named-foreign-type))
  (name type))

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

(defmethod unparse-type ((type foreign-built-in-type))
  "Returns the symbolic representation of a built-in type."
  (type-keyword type))

(defmethod print-object ((type foreign-built-in-type) stream)
  "Print a FOREIGN-TYPE instance to STREAM unreadably."
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (type-keyword type))))

(defmacro define-built-in-foreign-type (keyword)
  "Defines a built-in foreign-type."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (notice-foreign-type
      ,keyword (make-instance 'foreign-built-in-type :type-keyword ,keyword))))

;;;# Foreign Pointer Types

(defclass foreign-pointer-type (foreign-built-in-type)
  ((pointer-type
    ;; Type of object pointed at by this pointer, or nil for an
    ;; untyped (void) pointer.
    :initform nil
    :initarg :pointer-type
    :accessor pointer-type))
  (:default-initargs :type-keyword :pointer))

;;; Define the type parser for the :POINTER type.  If no type argument
;;; is provided, a void pointer will be created.
(let ((void-pointer (make-instance 'foreign-pointer-type)))
  (define-parse-method :pointer (&optional type)
    (if type
        (make-instance 'foreign-pointer-type :pointer-type (parse-type type))
        ;; A bit of premature optimization here.
        void-pointer)))

;;; Unparse a foreign pointer type when dumping to a fasl.
(defmethod unparse-type ((type foreign-pointer-type))
  (if (pointer-type type)
      `(:pointer ,(unparse-type (pointer-type type)))
      :pointer))

;;; Print a foreign pointer type unreadably in unparsed form.
(defmethod print-object ((type foreign-pointer-type) stream)
  (print-unreadable-object (type stream :type t :identity nil)
    (format stream "~S" (unparse-type type))))

;;;# Structure Type

(defclass foreign-struct-type (named-foreign-type)
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

;;;# Foreign Typedefs

(defclass foreign-type-alias (foreign-type)
  ((actual-type
    ;; The FOREIGN-TYPE instance this type is an alias for.
    :initarg :actual-type
    :accessor actual-type
    :initform (error "Must specify an ACTUAL-TYPE.")))
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

(defclass foreign-typedef (foreign-type-alias named-foreign-type)
  ())

(defun follow-typedefs (type)
  (if (eq (type-of type) 'foreign-typedef)
      (follow-typedefs (actual-type type))
      type))

;;;# Type Translators
;;;
;;; Type translation is done with generic functions at runtime for
;;; subclasses of ENHANCED-FOREIGN-TYPE/
;;;
;;; The main interface for defining type translations is through the
;;; generic functions TRANSLATE-{TO,FROM}-FOREIGN and
;;; FREE-TRANSLATED-OBJECT.

(defclass enhanced-foreign-type (foreign-type-alias)
  ((unparsed-type :accessor unparsed-type)))

;;; If actual-type isn't parsed already, let's parse it.  This way we
;;; don't have to export PARSE-TYPE and users don't have to worry
;;; about this in DEFINE-FOREIGN-TYPE or DEFINE-PARSE-METHOD.
(defmethod initialize-instance :after ((type enhanced-foreign-type) &key)
  (unless (typep (actual-type type) 'foreign-type)
    (setf (actual-type type) (parse-type (actual-type type)))))

(defmethod unparse-type ((type enhanced-foreign-type))
  (unparsed-type type))

;;; Checks NAMEs, not object identity.
(defun check-for-typedef-cycles (type)
  (let ((seen (make-hash-table :test 'eq)))
    (labels ((%check (cur-type)
               (when (typep cur-type 'foreign-typedef)
                 (when (gethash (name cur-type) seen)
                   (error "Detected cycle in type ~S." type))
                 (setf (gethash (name cur-type) seen) t)
                 (%check (actual-type cur-type)))))
      (%check type))))

;;; Only now we define PARSE-TYPE because it needs to do some extra
;;; work for ENHANCED-FOREIGN-TYPES.
(defun parse-type (type)
  (let* ((spec (ensure-list type))
         (ptype (apply (find-type-parser (car spec)) (cdr spec))))
    (when (typep ptype 'foreign-typedef)
      (check-for-typedef-cycles ptype))
    (when (typep ptype 'enhanced-foreign-type)
      (setf (unparsed-type ptype) type))
    ptype))

(defun canonicalize-foreign-type (type)
  "Convert TYPE to a built-in type by following aliases.
Signals an error if the type cannot be resolved."
  (canonicalize (parse-type type)))

;;; Translate VALUE to a foreign object of the type represented by
;;; TYPE, which will be a subclass of ENHANCED-FOREIGN-TYPE.  Returns
;;; the foreign value and an optional second value which will be
;;; passed to FREE-TRANSLATED-OBJECT as the PARAM argument.
(defgeneric translate-to-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    value))

;;; Similar to TRANSLATE-TO-FOREIGN, used exclusively by
;;; (SETF FOREIGN-STRUCT-SLOT-VALUE).
(defgeneric translate-aggregate-to-foreign (ptr value type))

;;; Translate the foreign object VALUE from the type repsented by
;;; TYPE, which will be a subclass of ENHANCED-FOREIGN-TYPE.  Returns
;;; the converted Lisp value.
(defgeneric translate-from-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    value))

;;; Free an object allocated by TRANSLATE-TO-FOREIGN.  VALUE is a
;;; foreign object of the type represented by TYPE, which will be a
;;; ENHANCED-FOREIGN-TYPE subclass.  PARAM, if present, contains the
;;; second value returned by TRANSLATE-TO-FOREIGN, and is used to
;;; communicate between the two functions.
;;;
;;; FIXME: I don't think this PARAM argument is necessary anymore
;;; because the TYPE object can contain that information. [2008-12-31 LO]
(defgeneric free-translated-object (value type param)
  (:method (value type param)
    (declare (ignore value type param))))

;;;## Macroexpansion Time Translation
;;;
;;; The following EXPAND-* generic functions are similar to their
;;; TRANSLATE-* counterparts but are usually called at macroexpansion
;;; time. They offer a way to optimize the runtime translators.

;;; This special variable is bound by the various :around methods
;;; below to the respective form generated by the above %EXPAND-*
;;; functions.  This way, an expander can "bail out" by calling the
;;; next method.  All 6 of the below-defined GFs have a default method
;;; that simply answers the rtf bound by the default :around method.
(defvar *runtime-translator-form*)

;;; EXPAND-FROM-FOREIGN

(defgeneric expand-from-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    value))

(defmethod expand-from-foreign :around (value (type enhanced-foreign-type))
  (let ((*runtime-translator-form* `(translate-from-foreign ,value ,type)))
    (call-next-method)))

(defmethod expand-from-foreign (value (type enhanced-foreign-type))
  (declare (ignore value))
  *runtime-translator-form*)

;;; EXPAND-TO-FOREIGN

;; The second return value is used to tell EXPAND-TO-FOREIGN-DYN that
;; an unspecialized method was called.
(defgeneric expand-to-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    (values value t)))

(defmethod expand-to-foreign :around (value (type enhanced-foreign-type))
  (let ((*runtime-translator-form*
         `(values (translate-to-foreign ,value ,type))))
    (call-next-method)))

(defmethod expand-to-foreign (value (type enhanced-foreign-type))
  (declare (ignore value))
  (values *runtime-translator-form* t))

;;; EXPAND-TO-FOREIGN-DYN

(defgeneric expand-to-foreign-dyn (value var body type)
  (:method (value var body type)
    (declare (ignore type))
    `(let ((,var ,value)) ,@body)))

(defmethod expand-to-foreign-dyn :around
    (value var body (type enhanced-foreign-type))
  (let ((*runtime-translator-form*
         (with-unique-names (param)
           `(multiple-value-bind (,var ,param)
                (translate-to-foreign ,value ,type)
              (unwind-protect
                   (progn ,@body)
                (free-translated-object ,var ,type ,param))))))
    (call-next-method)))

;;; If this method is called it means the user hasn't defined a
;;; to-foreign-dyn expansion, so we use the to-foreign expansion.
;;;
;;; However, we do so *only* if there's a specialized
;;; EXPAND-TO-FOREIGN for TYPE because otherwise we want to use the
;;; above *RUNTIME-TRANSLATOR-FORM* which includes a call to
;;; FREE-TRANSLATED-OBJECT.  (Or else there would occur no translation
;;; at all.)
(defmethod expand-to-foreign-dyn (value var body (type enhanced-foreign-type))
  (multiple-value-bind (expansion default-etp-p)
      (expand-to-foreign value type)
    (if default-etp-p
        *runtime-translator-form*
        `(let ((,var ,expansion))
           ,@body))))

;;; User interface for converting values from/to foreign using the
;;; type translators.  The compiler macros use the expanders when
;;; possible.

(defun convert-to-foreign (value type)
  (translate-to-foreign value (parse-type type)))

(define-compiler-macro convert-to-foreign (value type)
  (if (constantp type)
      (expand-to-foreign value (parse-type (eval type)))
      `(translate-to-foreign ,value (parse-type ,type))))

(defun convert-from-foreign (value type)
  (translate-from-foreign value (parse-type type)))

(define-compiler-macro convert-from-foreign (value type)
  (if (constantp type)
      (expand-from-foreign value (parse-type (eval type)))
      `(translate-from-foreign ,value (parse-type ,type))))

(defun free-converted-object (value type param)
  (free-translated-object value (parse-type type) param))

;;;# Enhanced typedefs

(defclass enhanced-typedef (foreign-typedef)
  ())

(defmethod translate-to-foreign (value (type enhanced-typedef))
  (translate-to-foreign value (actual-type type)))

(defmethod translate-from-foreign (value (type enhanced-typedef))
  (translate-from-foreign value (actual-type type)))

(defmethod free-translated-object (value (type enhanced-typedef) param)
  (free-translated-object value (actual-type type) param))

(defmethod expand-from-foreign (value (type enhanced-typedef))
  (expand-from-foreign value (actual-type type)))

(defmethod expand-to-foreign (value (type enhanced-typedef))
  (expand-to-foreign value (actual-type type)))

(defmethod expand-to-foreign-dyn (value var body (type enhanced-typedef))
  (expand-to-foreign-dyn value var body (actual-type type)))

;;;# User-defined Types and Translations.

(defmacro define-foreign-type (name supers slots &rest options)
  (multiple-value-bind (new-options simple-parser actual-type initargs)
      (let ((keywords '(:simple-parser :actual-type :default-initargs)))
        (apply #'values
               (remove-if (lambda (opt) (member (car opt) keywords)) options)
               (mapcar (lambda (kw) (cdr (assoc kw options))) keywords)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (defclass ,name ,(or supers '(enhanced-foreign-type))
         ,slots
         (:default-initargs ,@(when actual-type `(:actual-type ',actual-type))
             ,@initargs)
         ,@new-options)
       ,(when simple-parser
          `(define-parse-method ,(car simple-parser) (&rest args)
             (apply #'make-instance ',name args)))
       ',name)))

(defmacro defctype (name base-type &optional documentation)
  "Utility macro for simple C-like typedefs."
  (declare (ignore documentation))
  (warn-if-kw-or-belongs-to-cl name)
  (let* ((btype (parse-type base-type))
         (dtype (if (typep btype 'enhanced-foreign-type)
                    'enhanced-typedef
                    'foreign-typedef)))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (notice-foreign-type
        ',name (make-instance ',dtype :name ',name :actual-type ,btype)))))

;;; For Verrazano.  We memoize the type this way to help detect cycles.
(defmacro defctype* (name base-type)
  "Like DEFCTYPE but defers instantiation until parse-time."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (let (memoized-type)
       (define-parse-method ,name ()
         (unless memoized-type
           (setf memoized-type (make-instance 'foreign-typedef :name ',name
                                              :actual-type nil)
                 (actual-type memoized-type) (parse-type ',base-type)))
         memoized-type))))
