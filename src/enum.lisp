;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; enum.lisp --- Defining foreign constants as Lisp keywords.
;;;
;;; Copyright (C) 2005-2006, James Bielman  <jamesjb@jamesjb.com>
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

(defclass foreign-enum (foreign-typedef enhanced-foreign-type)
  ((keyword-values
    :initform (make-hash-table :test 'eq)
    :reader keyword-values)
   (value-keywords
    :initform (make-hash-table)
    :reader value-keywords))
  (:documentation "Describes a foreign enumerated type."))

(defun make-foreign-enum (type-name base-type values)
  "Makes a new instance of the foreign-enum class."
  (let ((type (make-instance 'foreign-enum :name type-name
                             :actual-type (parse-type base-type)))
        (default-value 0))
    (dolist (pair values)
      (destructuring-bind (keyword &optional (value default-value))
          (ensure-list pair)
        (check-type keyword keyword)
        (check-type value integer)
        (if (gethash keyword (keyword-values type))
            (error "A foreign enum cannot contain duplicate keywords: ~S."
                   keyword)
            (setf (gethash keyword (keyword-values type)) value))
        ;; This is completely arbitrary behaviour: we keep the last we
        ;; value->keyword mapping. I suppose the opposite would be
        ;; just as good (keeping the first). Returning a list with all
        ;; the keywords might be a solution too? Suggestions
        ;; welcome. --luis
        (setf (gethash value (value-keywords type)) keyword)
        (setq default-value (1+ value))))
    type))

(defmacro defcenum (name-and-options &body enum-list)
  "Define an foreign enumerated type."
  (discard-docstring enum-list)
  (destructuring-bind (name &optional (base-type :int))
      (ensure-list name-and-options)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (notice-foreign-type
        ',name (make-foreign-enum ',name ',base-type ',enum-list)))))

(defun hash-keys-to-list (ht)
  (loop for k being the hash-keys in ht collect k))

(defun foreign-enum-keyword-list (enum-type)
  "Return a list of KEYWORDS defined in ENUM-TYPE."
  (hash-keys-to-list (keyword-values (parse-type enum-type))))

;;; These [four] functions could be good canditates for compiler macros
;;; when the value or keyword is constant.  I am not going to bother
;;; until someone has a serious performance need to do so though. --jamesjb
(defun %foreign-enum-value (type keyword &key errorp)
  (check-type keyword keyword)
  (or (gethash keyword (keyword-values type))
      (when errorp
        (error "~S is not defined as a keyword for enum type ~S."
               keyword type))))

(defun foreign-enum-value (type keyword &key (errorp t))
  "Convert a KEYWORD into an integer according to the enum TYPE."
  (let ((type-obj (parse-type type)))
    (if (not (typep type-obj 'foreign-enum))
      (error "~S is not a foreign enum type." type)
      (%foreign-enum-value type-obj keyword :errorp errorp))))

(defun %foreign-enum-keyword (type value &key errorp)
  (check-type value integer)
  (or (gethash value (value-keywords type))
      (when errorp
        (error "~S is not defined as a value for enum type ~S."
               value type))))

(defun foreign-enum-keyword (type value &key (errorp t))
  "Convert an integer VALUE into a keyword according to the enum TYPE."
  (let ((type-obj (parse-type type)))
    (if (not (typep type-obj 'foreign-enum))
        (error "~S is not a foreign enum type." type)
        (%foreign-enum-keyword type-obj value :errorp errorp))))

(defmethod translate-to-foreign (value (type foreign-enum))
  (if (keywordp value)
      (%foreign-enum-value type value :errorp t)
      value))

(defmethod translate-from-foreign (value (type foreign-enum))
  (%foreign-enum-keyword type value :errorp t))

;;;# Foreign Bitfields as Lisp keywords
;;;
;;; DEFBITFIELD is an abstraction similar to the one provided by DEFCENUM.
;;; With some changes to DEFCENUM, this could certainly be implemented on
;;; top of it.

(defclass foreign-bitfield (foreign-typedef enhanced-foreign-type)
  ((symbol-values
    :initform (make-hash-table :test 'eq)
    :reader symbol-values)
   (value-symbols
    :initform (make-hash-table)
    :reader value-symbols))
  (:documentation "Describes a foreign bitfield type."))

(defun make-foreign-bitfield (type-name base-type values)
  "Makes a new instance of the foreign-bitfield class."
  (let ((type (make-instance 'foreign-bitfield :name type-name
                             :actual-type (parse-type base-type)))
        (bit-floor 1))
    (dolist (pair values)
      ;; bit-floor rule: find the greatest single-bit int used so far,
      ;; and store its left-shift
      (destructuring-bind (symbol &optional
                           (value (prog1 bit-floor
                                    (setf bit-floor (ash bit-floor 1)))
                                  value-p))
          (ensure-list pair)
        (check-type symbol symbol)
        (when value-p
          (check-type value integer)
          (when (and (>= value bit-floor) (single-bit-p value))
            (setf bit-floor (ash value 1))))
        (if (gethash symbol (symbol-values type))
            (error "A foreign bitfield cannot contain duplicate symbols: ~S."
                   symbol)
            (setf (gethash symbol (symbol-values type)) value))
        (push symbol (gethash value (value-symbols type)))))
    type))

(defmacro defbitfield (name-and-options &body masks)
  "Define an foreign enumerated type."
  (discard-docstring masks)
  (destructuring-bind (name &optional (base-type :int))
      (ensure-list name-and-options)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (notice-foreign-type
        ',name (make-foreign-bitfield ',name ',base-type ',masks)))))

(defun foreign-bitfield-symbol-list (bitfield-type)
  "Return a list of SYMBOLS defined in BITFIELD-TYPE."
  (hash-keys-to-list (symbol-values (parse-type bitfield-type))))

(defun %foreign-bitfield-value (type symbols)
  (reduce #'logior symbols
          :key (lambda (symbol)
                 (check-type symbol symbol)
                 (or (gethash symbol (symbol-values type))
                     (error "~S is not a valid symbol for bitfield type ~S."
                            symbol type)))))

(defun foreign-bitfield-value (type symbols)
  "Convert a list of symbols into an integer according to the TYPE bitfield."
  (let ((type-obj (parse-type type)))
    (if (not (typep type-obj 'foreign-bitfield))
      (error "~S is not a foreign bitfield type." type)
      (%foreign-bitfield-value type-obj symbols))))

(defun %foreign-bitfield-symbols (type value)
  (check-type value integer)
  (loop for mask being the hash-keys in (value-symbols type)
            using (hash-value symbols)
        when (= (logand value mask) mask)
        append symbols))

(defun foreign-bitfield-symbols (type value)
  "Convert an integer VALUE into a list of matching symbols according to
the bitfield TYPE."
  (let ((type-obj (parse-type type)))
    (if (not (typep type-obj 'foreign-bitfield))
        (error "~S is not a foreign bitfield type." type)
        (%foreign-bitfield-symbols type-obj value))))

(defmethod translate-to-foreign (value (type foreign-bitfield))
  (if (integerp value)
      value
      (%foreign-bitfield-value type (ensure-list value))))

(defmethod translate-from-foreign (value (type foreign-bitfield))
  (%foreign-bitfield-symbols type value))
