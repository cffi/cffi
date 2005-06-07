;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; functions.lisp --- High-level interface to foreign functions.
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

;;;# Calling Foreign Functions
;;;
;;; FOREIGN-FUNCALL is the main primitive for calling foreign
;;; functions.  It converts each argument based on the installed
;;; translators for its type, then passes the resulting list to
;;; CFFI-SYS:%FOREIGN-FUNCALL.

;; This macro is pretty darn hairy; patches welcome to refactor!
(defmacro foreign-funcall-1 (name in-args done-args)
  "Helper macro for FOREIGN-FUNCALL*."
  (flet ((convert-type (type)
           (canonicalize-foreign-type type))
         (append-done-args (&rest args)
           (append args done-args)))
    (cond
      ;; All arguments have been translated---perform the call.
      ((null in-args)
       `(%foreign-funcall ,name ,@(reverse done-args)))
      ;; One argument remains---it is the return value.
      ((null (cdr in-args))
       (let ((sym (gensym))
             (type (car in-args)))
         `(with-object-translated
              (,sym (foreign-funcall-1
                     ,name nil
                     ,(append-done-args (convert-type type)))
                    ,type :result)
            ,sym)))
      ;; More than one argument is available---translate the first
      ;; argument/type pair and recurse.
      (t
       (let ((sym (gensym))
             (type (first in-args))
             (value (second in-args)))
         `(with-object-translated (,sym ,value ,type :in)
            (foreign-funcall-1
             ,name ,(cddr in-args)
             ,(append-done-args sym (convert-type type)))))))))

(defmacro foreign-funcall (name &rest args)
  "Wrapper around FOREIGN-FUNCALL that translates its arguments."
  `(foreign-funcall-1 ,name ,args nil))

;;;# Defining Foreign Functions
;;;
;;; The DEFCFUN macro provides a declarative interface for defining
;;; Lisp functions that call foreign functions.

(defun lisp-function-name (name)
  "Return the Lisp function name for foreign function NAME."
  (etypecase name
    (list (second name))
    (string (intern (string-upcase (substitute #\- #\_ name))))))

(defun foreign-function-name (name)
  "Return the foreign function name of NAME."
  (etypecase name
    (list (first name))
    (string name)))

(defmacro defcfun (name return-type &body args)
  "Defines a Lisp function that calls a foreign function."
  (let ((lisp-name (lisp-function-name name))
        (foreign-name (foreign-function-name name)))
    `(defun ,lisp-name ,(mapcar #'car args)
       (foreign-funcall 
        ,foreign-name ,@(mapcan #'reverse args) ,return-type))))
