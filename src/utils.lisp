;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; utils.lisp --- Various utilities.
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

(in-package #:cl-user)

(defpackage #:cffi-utils
  (:use #:common-lisp)
  (:export #:discard-docstring
           #:parse-body
           #:with-unique-names
           #:once-only
           #:mklist
           #:make-gensym-list
           #:symbolicate
           #:let-when
           #:bif
           #:post-incf))

(in-package #:cffi-utils)

;;;# General Utilities

;;; frodef's, see: http://paste.lisp.org/display/2771#1
(defmacro post-incf (place &optional (delta 1) &environment env)
  "Increment PLACE by DELTA and return its previous value."
  (multiple-value-bind (dummies vals new setter getter)
      (get-setf-expansion place env)
    `(let* (,@(mapcar #'list dummies vals) (,(car new) ,getter))
       (prog1 ,(car new)
         (setq ,(car new) (+ ,(car new) ,delta))
         ,setter))))

;;; On Lisp, IIRC.
(defun mklist (x)
  "Make into list if atom."
  (if (listp x) x (list x)))

;;; My own, hah!
(defmacro discard-docstring (body-var)
  "Discards the first element of the list in body-var if it's a
string and the only element."
  `(when (and (stringp (car ,body-var)) (cdr ,body-var))
     (pop ,body-var)))

;;; Parse a body of code, removing an optional documentation string
;;; and declaration forms.  Returns the actual body, docstring, and
;;; declarations as three multiple values.
(defun parse-body (body)
  (let ((docstring nil)
        (declarations nil))
    (when (and (stringp (car body)) (cdr body))
      (setf docstring (pop body)))
    (loop while (and (consp (car body)) (eql (caar body) 'cl:declare))
          do (push (pop body) declarations))
    (values body docstring (nreverse declarations))))

;;; LET-IF (renamed to BIF) and LET-WHEN taken from KMRCL
(defmacro let-when ((var test-form) &body body)
  `(let ((,var ,test-form))
      (when ,var ,@body)))

(defmacro bif ((var test-form) if-true &optional if-false)
  `(let ((,var ,test-form))
      (if ,var ,if-true ,if-false)))

;;; ONCE-ONLY macro taken from PAIP
(defun starts-with (list x)
  "Is x a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun side-effect-free? (exp)
  "Is exp a constant, variable, or function,
  or of the form (THE type x) where x is side-effect-free?"
  (or (atom exp) (constantp exp)
      (starts-with exp 'function)
      (and (starts-with exp 'the)
           (side-effect-free? (third exp)))))

(defmacro once-only (variables &rest body)
    "Returns the code built by BODY.  If any of VARIABLES
  might have side effects, they are evaluated once and stored
  in temporary variables that are then passed to BODY."
    (assert (every #'symbolp variables))
    (let ((temps nil))
      (dotimes (i (length variables)) (push (gensym "ONCE") temps))
      `(if (every #'side-effect-free? (list .,variables))
   (progn .,body)
   (list 'let
    ,`(list ,@(mapcar #'(lambda (tmp var)
                `(list ',tmp ,var))
            temps variables))
    (let ,(mapcar #'(lambda (var tmp) `(,var ',tmp))
             variables temps)
      .,body)))))

;;;; The following utils were taken from SBCL's
;;;; src/code/*-extensions.lisp

;;; Automate an idiom often found in macros:
;;;   (LET ((FOO (GENSYM "FOO"))
;;;         (MAX-INDEX (GENSYM "MAX-INDEX-")))
;;;     ...)
;;;
;;; "Good notation eliminates thought." -- Eric Siggia
;;;
;;; Incidentally, this is essentially the same operator which
;;; _On Lisp_ calls WITH-GENSYMS.
(defmacro with-unique-names (symbols &body body)
  `(let ,(mapcar (lambda (symbol)
                   (let* ((symbol-name (symbol-name symbol))
                          (stem (if (every #'alpha-char-p symbol-name)
                                    symbol-name
                                    (concatenate 'string symbol-name "-"))))
                     `(,symbol (gensym ,stem))))
                 symbols)
     ,@body))

(defun make-gensym-list (n)
  "Return a list of N gensyms."
  (loop repeat n collect (gensym)))

(defun symbolicate (&rest things)
  "Concatenate together the names of some strings and symbols,
producing a symbol in the current package."
  (let* ((length (reduce #'+ things
                         :key (lambda (x) (length (string x)))))
         (name (make-array length :element-type 'character)))
    (let ((index 0))
      (dolist (thing things (values (intern name)))
        (let* ((x (string thing))
               (len (length x)))
          (replace name x :start1 index)
          (incf index len))))))

;(defun deprecation-warning (bad-name &optional good-name)
;  (warn "using deprecated ~S~@[, should use ~S instead~]"
;        bad-name
;        good-name))

;;; Anaphoric macros
;(defmacro awhen (test &body body)
;  `(let ((it ,test))
;     (when it ,@body)))

;(defmacro acond (&rest clauses)
;  (if (null clauses)
;      `()
;      (destructuring-bind ((test &body body) &rest rest) clauses
;        (once-only (test)
;          `(if ,test
;               (let ((it ,test)) (declare (ignorable it)),@body)
;               (acond ,@rest))))))
