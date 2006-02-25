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

(defgeneric unparse (type-name type-class)
  (:documentation
   "Unparse FOREIGN-TYPE to a type specification (symbol or list)."))

(defgeneric translate-p (foreign-type)
  (:documentation
   "Return true if type translators should run on FOREIGN-TYPE."))

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
  `(parse-type ',(unparse-type type)))

(defun canonicalize-foreign-type (type)
  "Convert TYPE to a built-in type by following aliases.
Signals an error if the type cannot be resolved."
  (canonicalize (parse-type type)))

(defmethod unparse (name (type foreign-type))
  "Default method to unparse TYPE to its name."
  (declare (ignore name))
  (name type))

(defun unparse-type (type)
  "Unparse a foreign type to a symbol or list type spec."
  (unparse (name type) type))

(defmethod foreign-type-size (type)
  "Return the size in bytes of a foreign type."
  (foreign-type-size (parse-type type)))

(defmethod translate-p ((type foreign-type))
  "By default, types will be translated."
  t)

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

(defmethod translate-p ((type foreign-built-in-type))
  "Built-in types are never translated."
  nil)

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
    :accessor actual-type)
   (translate-p
    ;; If true, this type should be translated (the default).
    :initform t
    :initarg :translate-p
    :accessor translate-p))
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

;;;# Type Translators
;;;
;;; Type translation is now done with generic functions at runtime.
;;;
;;; The main internal interface to type translation is through the
;;; generic functions TRANSLATE-TYPE-{TO,FROM}-FOREIGN and
;;; FREE-TYPE-TRANSLATED-OBJECT.  These should be specialized for
;;; subclasses of FOREIGN-TYPE requiring translation.
;;;
;;; User-defined type translators are defined by specializing
;;; additional methods that are called by the internal methods
;;; specialized on FOREIGN-TYPEDEF.  These methods dispatch on the
;;; name of the type.

;;; Translate VALUE to a foreign object of the type represented by
;;; TYPE, which will be a subclass of FOREIGN-TYPE.  Returns the
;;; foreign value and an optional second value which will be passed to
;;; FREE-TYPE-TRANSLATED-OBJECT as the PARAM argument.
(defgeneric translate-type-to-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    value))

;;; Translate the foreign object VALUE from the type repsented by
;;; TYPE, which will be a subclass of FOREIGN-TYPE.  Returns the
;;; converted Lisp value.
(defgeneric translate-type-from-foreign (value type)
  (:method (value type)
    (declare (ignore type))
    value))

;;; Free an object allocated by TRANSLATE-TYPE-TO-FOREIGN.  VALUE is a
;;; foreign object of the type represented by TYPE, which will be a
;;; FOREIGN-TYPE subclass.  PARAM, if present, contains the second
;;; value returned by TRANSLATE-TYPE-TO-FOREIGN, and is used to
;;; communicate between the two functions.
(defgeneric free-type-translated-object (value type param)
  (:method (value type param)
    (declare (ignore value type param))))

;;;## Translations for Typedefs
;;;
;;; By default, the translation methods for type definitions delegate
;;; to the translation methods for the ACTUAL-TYPE of the typedef.
;;;
;;; The user is allowed to intervene in this process by specializing
;;; TRANSLATE-TO-FOREIGN, TRANSLATE-FROM-FOREIGN, and
;;; FREE-TRANSLATED-OBJECT on the name of the typedef.

;;; Exported hook method allowing specific typedefs to define custom
;;; translators to convert VALUE to the foreign type named by NAME.
(defgeneric translate-to-foreign (value name)
  (:method (value name)
    (declare (ignore name))
    value))

;;; Exported hook method allowing specific typedefs to define custom
;;; translators to convert VALUE from the foreign type named by NAME.
(defgeneric translate-from-foreign (value name)
  (:method (value name)
    (declare (ignore name))
    value))

;;; Exported hook method allowing specific typedefs to free objects of
;;; type NAME allocated by TRANSLATE-TO-FOREIGN.
(defgeneric free-translated-object (value name param)
  (:method (value name param)
    (declare (ignore value name param))))

;;; Default translator to foreign for typedefs.  We build a list out
;;; of the second value returned from each translator so we can pass
;;; each parameter to the appropriate free method when freeing the
;;; object.
(defmethod translate-type-to-foreign (value (type foreign-typedef))
  (multiple-value-bind (value param)
      (translate-to-foreign value (name type))
    (multiple-value-bind (new-value new-param)
        (translate-type-to-foreign value (actual-type type))
      (values new-value (cons param new-param)))))

;;; Default translator from foreign for typedefs.
(defmethod translate-type-from-foreign (value (type foreign-typedef))
  (translate-from-foreign
   (translate-type-from-foreign value (actual-type type))
   (name type)))

;;; Default method for freeing translated foreign typedefs.  PARAM
;;; will actually be a list of parameters to pass to each translator
;;; method as returned by TRANSLATE-TYPE-TO-FOREIGN.
(defmethod free-type-translated-object (value (type foreign-typedef) param)
  (free-translated-object value (name type) (car param))
  (free-type-translated-object value (actual-type type) (cdr param)))

;;;## Macroexpansion Time Translation
;;;
;;; The following expand-* generic functions are similar to their
;;; translate-* counterparts but are usually called at macroexpansion
;;; time. They offer a way to optimize the runtime translators.
;;;
;;; The default methods expand to forms calling the runtime translators
;;; unless TRANSLATE-P returns NIL for the type.

(defun %expand-type-to-foreign-dyn (value var body type)
  (with-unique-names (param)
    (if (translate-p type)
        `(multiple-value-bind (,var ,param)
             (translate-type-to-foreign ,value ,type)
           (unwind-protect
                (progn ,@body)
             (free-type-translated-object ,var ,type ,param)))
        `(let ((,var ,value))
           ,@body))))

(defun %expand-type-to-foreign (value type)
  (if (translate-p type)
      `(translate-type-to-foreign ,value ,type)
      value))

(defun %expand-type-from-foreign (value type)
  (if (translate-p type)
      `(translate-type-from-foreign ,value ,type)
      `(values ,value)))

;;; This special variable is bound by the various :around methods below
;;; to the respective form generated by the above %EXPAND-* functions.
;;; This way, an expander can "bail out" by returning it.
(defvar *runtime-translator-form*)

(defgeneric expand-type-to-foreign-dyn (value var body type)
  (:method :around (value var body type)
    (let ((*runtime-translator-form*
           (%expand-type-to-foreign-dyn value var body type)))
      (call-next-method)))  
  (:method (value var body type)
    ;; If COMPUTE-APPLICABLE-METHODS only finds one method it's
    ;; the default one meaning that there is no to-foreign expander
    ;; therefore we return *RUNTIME-TRANSLATOR-FORM* instead.
    (if (< 1 (length (compute-applicable-methods
                      #'expand-type-to-foreign (list value type))))
        `(let ((,var ,(expand-type-to-foreign value type)))
           ,@body)
        *runtime-translator-form*)))

(defgeneric expand-type-to-foreign (value type)
  (:method :around (value type)
    (let ((*runtime-translator-form* (%expand-type-to-foreign value type)))
      (call-next-method)))
  (:method (value type)
    (declare (ignore value type))
    *runtime-translator-form*))

(defgeneric expand-type-from-foreign (value type)
  (:method :around (value type)
    (let ((*runtime-translator-form* (%expand-type-from-foreign value type)))
      (call-next-method)))
  (:method (value type)
    (declare (ignore value type))
    *runtime-translator-form*))

(defgeneric expand-to-foreign-dyn (value var body type))
(defgeneric expand-to-foreign (value type))
(defgeneric expand-from-foreign (value type))

(defun applicablep (gf &rest args)
  "Returns true if GF has any applicable methods for ARGS."
  (not (null (compute-applicable-methods gf args))))

(defmethod expand-type-to-foreign-dyn (value var body (type foreign-typedef))
  (cond ((applicablep #'expand-to-foreign-dyn value var body (name type))
         (expand-to-foreign-dyn value var body (name type)))
        ;; If there is to-foreign _expansion_ we use that.
        ((applicablep #'expand-to-foreign value (name type))
         `(let ((,var ,(expand-to-foreign value (name type))))
            ,@body))
        ;; Else...
        (t *runtime-translator-form*)))

(defmethod expand-type-to-foreign (value (type foreign-typedef))
  (if (applicablep #'expand-to-foreign value (name type))
      (expand-to-foreign value (name type))
      *runtime-translator-form*))

(defmethod expand-type-from-foreign (value (type foreign-typedef))
  (if (applicablep #'expand-from-foreign value (name type))
      (expand-from-foreign value (name type))
      *runtime-translator-form*))

;;; User interface for converting values from/to foreign using the
;;; type translators. Something doesn't feel right about this, makes
;;; me want to just export PARSE-TYPE...

(defun convert-to-foreign (value type)
  (translate-type-to-foreign value (parse-type type)))

(define-compiler-macro convert-to-foreign (value type)
  (if (constantp type)
      (expand-type-to-foreign value (parse-type (eval type)))
      `(translate-type-to-foreign ,value (parse-type ,type))))

(defun convert-from-foreign (value type)
  (translate-type-from-foreign value (parse-type type)))

(define-compiler-macro convert-from-foreign (value type)
  (if (constantp type)
      (expand-type-from-foreign value (parse-type (eval type)))
      `(translate-type-from-foreign ,value (parse-type ,type))))

(defun free-converted-object (value type param)
  (free-type-translated-object value type param))